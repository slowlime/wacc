use std::cmp::Ordering;
use std::iter::{once, successors};
use std::ops::{Index, IndexMut};

use indexmap::IndexMap;
use itertools::{EitherOrBoth, Itertools};

use crate::ast::ty::BuiltinClass;
use crate::util::define_byte_string;
use crate::{analysis, try_match};

use super::func::FullFuncName;
use super::instr::{FieldName, MethodName};
use super::mem::ArenaRef;
use super::value::Value;

type TyStorage<'a> = IndexMap<IrClassName<'a>, IrClass<'a>>;

define_byte_string! {
    pub struct IrClassName<'a>;
}

impl<'a> IrClassName<'a> {
    pub fn func_name(&self, method: MethodName<'a>) -> FullFuncName<'a> {
        FullFuncName::Method(*self, method)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodDefKind<'a> {
    /// The first definition of a method in the inheritance hierarchy.
    BaseDef,

    /// An override of a method defined in a superclass.
    Override(IrClassName<'a>),

    /// A method definition inherited from a superclass.
    Inherited(IrClassName<'a>),
}

#[derive(Debug, Clone)]
pub struct IrMethod<'a> {
    pub name: MethodName<'a>,
    pub class_name: IrClassName<'a>,
    pub ty: IrFuncRef<'a>,
    pub def_kind: MethodDefKind<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldDefKind<'a> {
    BaseDef,
    Inherited(IrClassName<'a>),
}

#[derive(Debug, Clone)]
pub struct IrField<'a> {
    pub name: FieldName<'a>,
    pub class_name: IrClassName<'a>,
    pub ty: MaybeSelfTy<'a>,
    pub def_kind: FieldDefKind<'a>,
}

#[derive(Debug)]
pub struct IrClass<'a> {
    pub name: IrClassName<'a>,
    pub parent: Option<IrClassName<'a>>,
    pub methods: IndexMap<MethodName<'a>, IrMethod<'a>>,
    pub fields: IndexMap<FieldName<'a>, IrField<'a>>,
    pub r#final: bool,
}

#[derive(Debug)]
pub struct IrClassBuilder<'a>(IrClass<'a>);

impl<'a> IrClassBuilder<'a> {
    pub fn new(name: IrClassName<'a>, parent: Option<&IrClass<'a>>, r#final: bool) -> Self {
        let mut this = Self(IrClass {
            name,
            parent: parent.map(|class| class.name),
            methods: Default::default(),
            fields: Default::default(),
            r#final,
        });

        if let Some(parent) = parent {
            assert!(!parent.r#final);
            this.inherit_definitions(parent);
        }

        this
    }

    fn inherit_definitions(&mut self, parent: &IrClass<'a>) {
        for (&name, method) in &parent.methods {
            let ty = if method.ty.has_self() {
                // update the `SubtypeOf` bound
                let first_param = match method.ty.args()[0] {
                    IrTy::Object(class, _) => IrTy::Object(class, DynTy::SubtypeOf(self.0.name)),
                    _ => unreachable!(),
                };
                let args = once(first_param)
                    .chain(method.ty.args.iter().skip(1).cloned())
                    .collect();

                IrFuncRef::new(args, Box::new(method.ty.ret().clone()))
            } else {
                method.ty.clone()
            };

            self.0.methods.insert(
                name,
                IrMethod {
                    name,
                    class_name: self.0.name,
                    ty,
                    def_kind: match method.def_kind {
                        MethodDefKind::Inherited(base_class) => {
                            MethodDefKind::Inherited(base_class)
                        }

                        _ => MethodDefKind::Inherited(parent.name),
                    },
                },
            );
        }

        for (&name, field) in &parent.fields {
            self.0.fields.insert(
                name,
                IrField {
                    name,
                    class_name: self.0.name,
                    ty: field.ty.clone(),
                    def_kind: match field.def_kind {
                        FieldDefKind::Inherited(base_class) => FieldDefKind::Inherited(base_class),

                        _ => FieldDefKind::Inherited(parent.name),
                    },
                },
            );
        }
    }

    pub fn add_method(&mut self, name: MethodName<'a>, ty: IrFuncRef<'a>) -> &IrMethod<'a> {
        use indexmap::map::Entry;

        match self.0.methods.entry(name) {
            Entry::Vacant(entry) => entry.insert(IrMethod {
                name,
                class_name: self.0.name,
                ty,
                def_kind: MethodDefKind::BaseDef,
            }),

            Entry::Occupied(mut entry) => {
                let def_kind = match entry.get().def_kind {
                    MethodDefKind::Override(_) | MethodDefKind::BaseDef => {
                        panic!(
                            "Method {} is defined twice for the class {}",
                            name, self.0.name
                        );
                    }

                    MethodDefKind::Inherited(base_class) => MethodDefKind::Override(base_class),
                };

                assert_eq!(entry.get().ty, ty);

                entry.insert(IrMethod {
                    name,
                    class_name: self.0.name,
                    ty,
                    def_kind,
                });

                entry.into_mut()
            }
        }
    }

    pub fn with_method(mut self, name: MethodName<'a>, ty: IrFuncRef<'a>) -> Self {
        self.add_method(name, ty);

        self
    }

    pub fn add_field(&mut self, name: FieldName<'a>, ty: MaybeSelfTy<'a>) -> &IrField<'a> {
        use indexmap::map::Entry;

        match self.0.fields.entry(name) {
            Entry::Vacant(entry) => entry.insert(IrField {
                name,
                class_name: self.0.name,
                ty,
                def_kind: FieldDefKind::BaseDef,
            }),

            Entry::Occupied(_) => {
                panic!("Field {} is defined twice for class {}", name, self.0.name);
            }
        }
    }

    pub fn with_field(mut self, name: FieldName<'a>, ty: MaybeSelfTy<'a>) -> Self {
        self.add_field(name, ty);

        self
    }

    pub fn get_method(&self, name: MethodName<'a>) -> Option<&IrMethod<'a>> {
        self.0.methods.get(&name)
    }

    pub fn get_field(&self, name: FieldName<'a>) -> Option<&IrField<'a>> {
        self.0.fields.get(&name)
    }

    pub fn register<'t>(self, ty_registry: &'t mut IrTyRegistry<'a>) -> &'t mut IrClass<'a> {
        ty_registry.add_class(self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IrFuncRef<'a> {
    args: Vec<IrTy<'a>>,
    ret: Box<MaybeSelfTy<'a>>,
}

impl<'a> IrFuncRef<'a> {
    pub fn new(args: Vec<IrTy<'a>>, ret: Box<MaybeSelfTy<'a>>) -> Self {
        assert!(args
            .iter()
            .all(|arg| arg.is_class_ty() || arg.is_primitive()));
        assert!(args.iter().skip(1).all(|arg| !arg.is_annotated()));

        match (args.first(), &*ret) {
            (None, MaybeSelfTy::SelfTy(_)) => panic!("a free function cannot return Self"),
            (None, MaybeSelfTy::Other(_)) => {}
            (Some(IrTy::Object(_, DynTy::Known(_) | DynTy::DynTyOf(_))), _) => {
                panic!("invalid dynamic type annotation")
            }
            (Some(&IrTy::Object(param, DynTy::SubtypeOf(_))), &MaybeSelfTy::SelfTy(ret)) => {
                assert_eq!(param, ret)
            }
            (Some(IrTy::Object(_, DynTy::Unknown)), MaybeSelfTy::SelfTy(_)) => {
                panic!("the first parameter must be annotated")
            }
            (Some(_), MaybeSelfTy::SelfTy(_)) => panic!("the first parameter has an invalid type"),
            (Some(IrTy::Object(_, _)), MaybeSelfTy::Other(_)) => {}
            (_, _) => unreachable!(),
        };

        Self { args, ret }
    }

    /// The function parameters. Only the first parameter may be `SelfTy`.
    pub fn args(&self) -> &[IrTy<'a>] {
        &self.args
    }

    /// The type of the returned value.
    ///
    /// If it is `SelfTy`, it's guaranteed to be equal to the type of the first parameter.
    pub fn ret(&self) -> &MaybeSelfTy<'a> {
        &self.ret
    }

    /// Returns `true` if `SelfTy` is used in the signature.
    pub fn has_self(&self) -> bool {
        self.args.len() > 0 && self.args[0].is_annotated()
    }

    pub fn bind(mut self, value: Option<(Value, DynTy<'a>)>) -> BoundFuncRef<'a> {
        assert!(!self.has_self() || value.is_some());

        let Some((value, value_dyn_ty)) = value else { return BoundFuncRef(self) };
        self.args[0] = self.args[0].clone().bind(value, value_dyn_ty);

        if self.ret.is_self_ty() {
            self.ret = Box::new(self.args[0].clone().into());
        }

        BoundFuncRef(self)
    }
}

macro_rules! maybe_self_ty {
    (self($ty:expr)) => {
        MaybeSelfTy::SelfTy($ty)
    };

    ($($ty:tt)+) => {
        MaybeSelfTy::Other($crate::ir::ty::object_ty!($($ty)+))
    };
}

pub(crate) use maybe_self_ty;

macro_rules! object_ty {
    (($ty:expr)[?]) => {
        object_ty!($ty)
    };
    (($ty:expr)[? <: $sub:expr]) => {
        IrTy::Object($ty, DynTy::SubtypeOf($sub))
    };
    (($ty:expr)[dyn $v:expr]) => {
        IrTy::Object($ty, DynTy::DynTyOf($v))
    };
    (($ty:expr)[$dyn:expr]) => {
        IrTy::Object($ty, DynTy::Known($dyn))
    };
    ($ty:expr) => {
        IrTy::Object($ty, DynTy::Unknown)
    };
}

pub(crate) use object_ty;

macro_rules! func_ref {
    (($($arg:tt)*) -> $($ret:tt)+) => (IrFuncRef::new(
        {
            #[allow(unused_mut)]
            let mut args = Vec::new();
            $crate::ir::ty::func_ref!(@args &mut args, $($arg)*);
            args
        },
        Box::new($crate::ir::ty::maybe_self_ty!($($ret)+)),
    ));

    (@args $args:expr,) => ();
    (@args $args:expr, self($ty:expr)$sub:tt) => ($args.push($crate::ir::ty::object_ty!(($ty)$sub)));
    (@args $args:expr, self($ty:expr)$sub:tt, $($rest:expr),*) => ({
        $args.push($crate::ir::ty::object_ty!(($ty)$sub));
        $crate::ir::ty::func_ref!(@args $args, $($rest),*);
    });

    (@args $args:expr, $arg:expr) => ($args.push($crate::ir::ty::object_ty!($arg)));
    (@args $args:expr, $arg:expr, $($rest:expr),*) => ({
        $args.push($crate::ir::ty::object_ty!($arg));
        $crate::ir::ty::func_ref!(@args $args, $($rest),*);
    });
}

pub(crate) use func_ref;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MaybeSelfTy<'a> {
    SelfTy(IrClassName<'a>),
    Other(IrTy<'a>),
}

impl<'a> MaybeSelfTy<'a> {
    pub fn is_self_ty(&self) -> bool {
        matches!(self, Self::SelfTy(_))
    }

    pub fn bind(self, value: Value, value_dyn_ty: DynTy<'a>) -> IrTy<'a> {
        match self {
            Self::SelfTy(name) => IrTy::Object(name, DynTy::Unknown.bind(value, value_dyn_ty)),
            Self::Other(ty) => ty.bind(value, value_dyn_ty),
        }
    }

    pub fn unwrap_ty(self) -> IrTy<'a> {
        match self {
            Self::SelfTy(_) => panic!("attempted to unwrap a SelfTy"),
            Self::Other(ty) => ty,
        }
    }
}

impl<'a> From<IrTy<'a>> for MaybeSelfTy<'a> {
    fn from(ty: IrTy<'a>) -> Self {
        Self::Other(ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoundFuncRef<'a>(IrFuncRef<'a>);

impl<'a> BoundFuncRef<'a> {
    pub fn func_ref(&self) -> &IrFuncRef<'a> {
        &self.0
    }

    pub fn args(&self) -> &[IrTy<'a>] {
        self.0.args()
    }

    pub fn ret(&self) -> &IrTy<'a> {
        match self.0.ret() {
            MaybeSelfTy::Other(ret) => ret,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DynTy<'a> {
    Known(IrClassName<'a>),
    Unknown,
    SubtypeOf(IrClassName<'a>),
    DynTyOf(Value),
}

impl<'a> DynTy<'a> {
    pub fn bind(self, value: Value, value_dyn_ty: Self) -> Self {
        use DynTy::*;

        match (self, value_dyn_ty) {
            (Known(_), _) => self,
            (_, Known(_)) => value_dyn_ty,
            (Unknown | SubtypeOf(_), DynTyOf(_)) => value_dyn_ty,
            (Unknown | SubtypeOf(_), _) => DynTyOf(value),
            (DynTyOf(_), _) => self,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IrTy<'a> {
    // unboxed types
    I32,
    Bool,
    Bytes,
    Unit,

    // boxed types
    Object(IrClassName<'a>, DynTy<'a>),
    FuncRef(BoundFuncRef<'a>),
}

impl<'a> IrTy<'a> {
    pub fn new_known(class: IrClassName<'a>) -> IrTy<'a> {
        IrTy::Object(class, DynTy::Known(class))
    }

    pub fn is_boxed(&self) -> bool {
        !self.is_primitive()
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, IrTy::I32 | IrTy::Bool | IrTy::Bytes | IrTy::Unit)
    }

    pub fn get_class(&self) -> Option<IrClassName<'a>> {
        try_match!(*self, Self::Object(class, _) => class)
    }

    pub fn get_dyn_ty(&self) -> Option<DynTy<'a>> {
        try_match!(*self, Self::Object(_, dyn_ty) => dyn_ty)
    }

    pub fn is_class_ty(&self) -> bool {
        matches!(self, Self::Object(_, _))
    }

    pub fn is_annotated(&self) -> bool {
        match self {
            Self::Object(_, DynTy::Unknown) => false,
            Self::Object(_, _) => true,
            _ => false,
        }
    }

    pub fn bind(self, value: Value, value_dyn_ty: DynTy<'a>) -> Self {
        match self {
            IrTy::Object(class, dyn_ty) => IrTy::Object(class, dyn_ty.bind(value, value_dyn_ty)),
            _ => self,
        }
    }

    pub fn base_ty_equals(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Object(lhs, _), Self::Object(rhs, _)) => lhs == rhs,
            _ => self == other,
        }
    }
}

impl<'a> From<BoundFuncRef<'a>> for IrTy<'a> {
    fn from(func_ref: BoundFuncRef<'a>) -> Self {
        Self::FuncRef(func_ref)
    }
}

pub struct IrTyRegistry<'a> {
    tys: TyStorage<'a>,
    arena: ArenaRef<'a>,

    pub object_class: IrClassName<'a>,
    pub int_class: IrClassName<'a>,
    pub string_class: IrClassName<'a>,
    pub bool_class: IrClassName<'a>,
    pub io_class: IrClassName<'a>,

    pub new_method: MethodName<'a>,
    pub init_method: MethodName<'a>,
    pub copy_method: MethodName<'a>,
    pub type_name_method: MethodName<'a>,
}

impl<'a> IrTyRegistry<'a> {
    pub fn new(arena: ArenaRef<'a>) -> Self {
        let mut this = Self {
            tys: TyStorage::new(),
            arena,

            object_class: arena.alloc(b"Object"),
            int_class: arena.alloc(b"Int"),
            string_class: arena.alloc(b"String"),
            bool_class: arena.alloc(b"Bool"),
            io_class: arena.alloc(b"IO"),

            new_method: arena.alloc(b"(new)"),
            init_method: arena.alloc(b"(init)"),
            copy_method: arena.alloc(b"(copy)"),
            type_name_method: arena.alloc(b"(type_name)"),
        };

        this.object_builder().register(&mut this);
        this.int_builder().register(&mut this);
        this.string_builder().register(&mut this);
        this.bool_builder().register(&mut this);
        this.io_builder().register(&mut this);

        this
    }

    fn add_class<'s>(&'s mut self, class: IrClass<'a>) -> &'s mut IrClass<'a> {
        use indexmap::map::Entry;

        match self.tys.entry(class.name) {
            Entry::Occupied(_) => {
                panic!("Class {} is defined twice", class.name);
            }

            Entry::Vacant(entry) => entry.insert(class),
        }
    }

    pub fn object_class(&self) -> &IrClass<'a> {
        &self.tys[&self.object_class]
    }

    pub fn int_class(&self) -> &IrClass<'a> {
        &self.tys[&self.int_class]
    }

    pub fn string_class(&self) -> &IrClass<'a> {
        &self.tys[&self.string_class]
    }

    pub fn bool_class(&self) -> &IrClass<'a> {
        &self.tys[&self.bool_class]
    }

    pub fn io_class(&self) -> &IrClass<'a> {
        &self.tys[&self.io_class]
    }

    pub fn get_class(&self, class_name: IrClassName<'a>) -> Option<&IrClass<'a>> {
        self.tys.get(&class_name)
    }

    pub fn get_builtin_class(&self, builtin: BuiltinClass) -> &IrClass<'a> {
        match builtin {
            BuiltinClass::IO => self.io_class(),
            BuiltinClass::Int => self.int_class(),
            BuiltinClass::Bool => self.bool_class(),
            BuiltinClass::String => self.string_class(),
            BuiltinClass::Object => self.object_class(),
        }
    }

    pub fn get_class_by_class_name(
        &self,
        class_name: &analysis::ClassName<'_>,
    ) -> Option<&IrClass<'a>> {
        use analysis::ClassName;

        match *class_name {
            ClassName::Named(ref name) => {
                let class_name = self.arena.alloc(name);
                self.get_class(class_name)
            }

            ClassName::Builtin(builtin) => Some(self.get_builtin_class(builtin)),
            ClassName::SelfType => panic!("SELF_TYPE is not a proper type"),
        }
    }

    pub fn to_boxed_ty(&self, ty: &IrTy<'a>) -> Option<IrTy<'a>> {
        Some(match ty {
            IrTy::I32 => IrTy::Object(self.int_class, DynTy::Known(self.int_class)),
            IrTy::Bool => IrTy::Object(self.bool_class, DynTy::Known(self.bool_class)),
            IrTy::Bytes => IrTy::Object(self.string_class, DynTy::Known(self.string_class)),
            IrTy::Unit => IrTy::Object(self.object_class, DynTy::Known(self.object_class)),
            _ => return None,
        })
    }

    pub fn to_unboxed_ty(&self, ty: &IrTy<'a>) -> Option<IrTy<'a>> {
        Some(match ty.get_class() {
            Some(name) if name == self.int_class => IrTy::I32,
            Some(name) if name == self.bool_class => IrTy::Bool,
            Some(name) if name == self.string_class => IrTy::Bytes,
            Some(name) if name == self.object_class => IrTy::Unit,
            _ => return None,
        })
    }

    pub fn is_subtype(&self, lhs: &IrTy<'a>, rhs: &IrTy<'a>) -> bool {
        match (lhs, rhs) {
            (_, _) if lhs == rhs => true,
            (_, _) if lhs.is_primitive() || rhs.is_primitive() => false,
            // not sure if wasm defines subtyping on function references;
            // for the sake of sanity let's presume it doesn't
            (IrTy::FuncRef(_), _) | (_, IrTy::FuncRef(_)) => false,
            (&IrTy::Object(lhs_class, _), &IrTy::Object(rhs_class, _)) => {
                self.compare_class_tys(lhs_class, rhs_class) == Some(Ordering::Less)
            }

            (_, _) => unreachable!(),
        }
    }

    pub fn compare_class_tys(
        &self,
        lhs_class: IrClassName<'a>,
        rhs_class: IrClassName<'a>,
    ) -> Option<Ordering> {
        let lhs_chain = self.inheritance_chain(lhs_class).collect::<Vec<_>>();
        let rhs_chain = self.inheritance_chain(rhs_class).collect::<Vec<_>>();

        let lhs_iter = lhs_chain.iter().rev();
        let rhs_iter = rhs_chain.iter().rev();

        lhs_iter
            .zip_longest(rhs_iter)
            .find_map(|entry| match entry {
                EitherOrBoth::Both(lhs, rhs) if lhs == rhs => None,
                EitherOrBoth::Both(_, _) => Some(None),
                EitherOrBoth::Left(_) => Some(Some(Ordering::Less)),
                EitherOrBoth::Right(_) => Some(Some(Ordering::Greater)),
            })
            .unwrap_or(Some(Ordering::Equal))
    }

    pub fn inheritance_chain<'s>(
        &'s self,
        lower_bound: IrClassName<'a>,
    ) -> impl Iterator<Item = IrClassName<'a>> + 's {
        successors(Some(lower_bound), |&class_name| self[class_name].parent)
    }

    fn object_builder<'s>(&'s self) -> IrClassBuilder<'a> {
        IrClassBuilder::new(self.object_class, None, false)
            .with_method(self.new_method, func_ref!(() -> self.object_class))
            .with_method(
                self.init_method,
                IrFuncRef::new(
                    vec![IrTy::Object(self.object_class, DynTy::SubtypeOf(self.object_class))],
                    Box::new(IrTy::Unit.into()),
                ),
            )
            .with_method(
                self.copy_method,
                func_ref!((self(self.object_class)[self.object_class]) -> self(self.object_class)),
            )
            .with_method(self.type_name_method, IrFuncRef::new(vec![], Box::new(IrTy::Bytes.into())))
            .with_method(
                self.arena.alloc(b"abort"),
                func_ref!((self(self.object_class)[? <: self.object_class]) -> self.object_class),
            )
            .with_method(
                self.arena.alloc(b"type_name"),
                func_ref!((self(self.object_class)[? <: self.object_class]) -> self.string_class),
            )
            .with_method(
                self.arena.alloc(b"copy"),
                func_ref!((self(self.object_class)[? <: self.object_class]) -> self(self.object_class)),
            )
    }

    fn int_builder<'s>(&'s self) -> IrClassBuilder<'a> {
        IrClassBuilder::new(self.int_class, Some(self.object_class()), true)
    }

    fn string_builder<'s>(&'s self) -> IrClassBuilder<'a> {
        // we pass String instead of SELF_TYPE as the first param because String is final
        IrClassBuilder::new(self.string_class, Some(self.object_class()), true)
            .with_method(
                self.arena.alloc(b"length"),
                func_ref!((self.string_class) -> self.int_class),
            )
            .with_method(
                self.arena.alloc(b"concat"),
                func_ref!((self.string_class, self.string_class) -> self.string_class),
            )
            .with_method(
                self.arena.alloc(b"substr"),
                func_ref!((self.string_class, self.int_class, self.int_class) -> self.string_class),
            )
    }

    fn bool_builder<'s>(&'s self) -> IrClassBuilder<'a> {
        IrClassBuilder::new(self.bool_class, Some(self.object_class()), true)
    }

    fn io_builder<'s>(&'s self) -> IrClassBuilder<'a> {
        IrClassBuilder::new(self.io_class, Some(self.object_class()), false)
            .with_method(
                self.arena.alloc(b"out_string"),
                func_ref!((self(self.io_class)[? <: self.io_class], self.string_class) -> self(self.io_class)),
            )
            .with_method(
                self.arena.alloc(b"out_int"),
                func_ref!((self(self.io_class)[? <: self.io_class], self.int_class) -> self(self.io_class)),
            )
            .with_method(
                self.arena.alloc(b"in_string"),
                func_ref!((self(self.io_class)[? <: self.io_class]) -> self.string_class),
            )
            .with_method(
                self.arena.alloc(b"in_int"),
                func_ref!((self(self.io_class)[? <: self.io_class]) -> self.int_class),
            )
    }
}

impl<'a> IrTyRegistry<'a> {
    pub fn get_method_base_def<'s>(&'s self, mut method: &'s IrMethod<'a>) -> &IrMethod<'a> {
        while let MethodDefKind::Override(class) = method.def_kind {
            method = &self[class].methods[&method.name];
        }

        method
    }

    pub fn get_field_base_def<'s>(&'s self, field: &'s IrField<'a>) -> &IrField<'a> {
        match field.def_kind {
            FieldDefKind::BaseDef => field,
            FieldDefKind::Inherited(class) => &self[class].fields[&field.name],
        }
    }
}

impl<'a> Index<IrClassName<'a>> for IrTyRegistry<'a> {
    type Output = IrClass<'a>;

    fn index(&self, index: IrClassName<'a>) -> &Self::Output {
        &self.tys[&index]
    }
}

impl<'a> IndexMut<IrClassName<'a>> for IrTyRegistry<'a> {
    fn index_mut(&mut self, index: IrClassName<'a>) -> &mut Self::Output {
        &mut self.tys[&index]
    }
}
