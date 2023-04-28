use std::ops::{Index, IndexMut};

use indexmap::IndexMap;

use crate::analysis;
use crate::ast::ty::BuiltinClass;
use crate::util::define_byte_string;

use super::instr::{FieldName, MethodName};
use super::mem::ArenaRef;

type TyStorage<'a> = IndexMap<IrClassName<'a>, IrClass<'a>>;

define_byte_string! {
    pub struct IrClassName<'a>;
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
}

#[derive(Debug)]
pub struct IrClassBuilder<'a>(IrClass<'a>);

impl<'a> IrClassBuilder<'a> {
    pub fn new(name: IrClassName<'a>, parent: Option<&IrClass<'a>>) -> Self {
        let mut this = Self(IrClass {
            name,
            parent: parent.map(|class| class.name),
            methods: Default::default(),
            fields: Default::default(),
        });

        if let Some(parent) = parent {
            this.inherit_definitions(parent);
        }

        this
    }

    fn inherit_definitions(&mut self, parent: &IrClass<'a>) {
        for (&name, method) in &parent.methods {
            self.0.methods.insert(
                name,
                IrMethod {
                    name,
                    class_name: self.0.name,
                    ty: method.ty.clone(),
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
    pub args: Vec<IrTy<'a>>,
    pub ret: Box<MaybeSelfTy<'a>>,
}

impl<'a> IrFuncRef<'a> {
    pub fn new<const N: usize>(args: [IrTy<'a>; N], ret: impl Into<MaybeSelfTy<'a>>) -> Self {
        Self {
            args: args.to_vec(),
            ret: Box::new(ret.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MaybeSelfTy<'a> {
    SelfTy,
    Other(IrTy<'a>),
}

impl<'a> MaybeSelfTy<'a> {
    pub fn unwrap_ty(self) -> IrTy<'a> {
        match self {
            Self::SelfTy => panic!("SELF_TYPE cannot be reified as a type"),
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
pub enum IrTy<'a> {
    // unboxed types
    I32,
    Bool,
    Bytes,
    Unit,

    // boxed types
    Object(Option<IrClassName<'a>>),
    FuncRef(IrFuncRef<'a>),
}

impl<'a> IrTy<'a> {
    pub fn class_ty(class: IrClassName<'a>) -> IrTy<'a> {
        Self::Object(Some(class))
    }

    pub fn object_ty() -> IrTy<'a> {
        Self::Object(None)
    }

    pub fn func_ref_ty(args: Vec<IrTy<'a>>, ret: Box<MaybeSelfTy<'a>>) -> IrTy<'a> {
        Self::FuncRef(IrFuncRef { args, ret })
    }

    pub fn is_boxed(&self) -> bool {
        !matches!(self, IrTy::I32 | IrTy::Bool | IrTy::Bytes | IrTy::Unit)
    }

    pub fn get_class(&self) -> Option<IrClassName<'a>> {
        match *self {
            Self::Object(class) => class,
            _ => None,
        }
    }
}

pub trait HasIrTy<'a> {
    fn ty(&self) -> &IrTy<'a>;
}

impl<'a> HasIrTy<'a> for IrTy<'a> {
    fn ty(&self) -> &IrTy<'a> {
        self
    }
}

pub struct IrTyRegistry<'a> {
    tys: TyStorage<'a>,
    arena: ArenaRef<'a>,

    object_class: IrClassName<'a>,
    int_class: IrClassName<'a>,
    string_class: IrClassName<'a>,
    bool_class: IrClassName<'a>,
    io_class: IrClassName<'a>,
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

    fn object_builder<'s>(&'s self) -> IrClassBuilder<'a> {
        IrClassBuilder::new(self.object_class, None)
            .with_method(
                self.arena.alloc(b"abort"),
                IrFuncRef::new([IrTy::class_ty(self.object_class)], IrTy::object_ty()),
            )
            .with_method(
                self.arena.alloc(b"type_name"),
                IrFuncRef::new(
                    [IrTy::class_ty(self.object_class)],
                    IrTy::class_ty(self.string_class),
                ),
            )
            .with_method(
                self.arena.alloc(b"copy"),
                IrFuncRef::new([IrTy::class_ty(self.object_class)], MaybeSelfTy::SelfTy),
            )
    }

    fn int_builder<'s>(&'s self) -> IrClassBuilder<'a> {
        IrClassBuilder::new(self.int_class, Some(self.object_class()))
    }

    fn string_builder<'s>(&'s self) -> IrClassBuilder<'a> {
        IrClassBuilder::new(self.string_class, Some(self.object_class()))
            .with_method(
                self.arena.alloc(b"length"),
                IrFuncRef::new(
                    [IrTy::class_ty(self.string_class)],
                    IrTy::class_ty(self.int_class),
                ),
            )
            .with_method(
                self.arena.alloc(b"concat"),
                IrFuncRef::new(
                    [
                        IrTy::class_ty(self.string_class),
                        IrTy::class_ty(self.string_class),
                    ],
                    IrTy::class_ty(self.string_class),
                ),
            )
            .with_method(
                self.arena.alloc(b"substr"),
                IrFuncRef::new(
                    [
                        IrTy::class_ty(self.string_class),
                        IrTy::class_ty(self.int_class),
                        IrTy::class_ty(self.int_class),
                    ],
                    IrTy::class_ty(self.string_class),
                ),
            )
    }

    fn bool_builder<'s>(&'s self) -> IrClassBuilder<'a> {
        IrClassBuilder::new(self.bool_class, Some(self.object_class()))
    }

    fn io_builder<'s>(&'s self) -> IrClassBuilder<'a> {
        IrClassBuilder::new(self.io_class, Some(self.object_class()))
            .with_method(
                self.arena.alloc(b"out_string"),
                IrFuncRef::new(
                    [
                        IrTy::class_ty(self.io_class),
                        IrTy::class_ty(self.string_class),
                    ],
                    MaybeSelfTy::SelfTy,
                ),
            )
            .with_method(
                self.arena.alloc(b"out_int"),
                IrFuncRef::new(
                    [
                        IrTy::class_ty(self.io_class),
                        IrTy::class_ty(self.int_class),
                    ],
                    MaybeSelfTy::SelfTy,
                ),
            )
            .with_method(
                self.arena.alloc(b"in_string"),
                IrFuncRef::new(
                    [IrTy::class_ty(self.io_class)],
                    IrTy::class_ty(self.string_class),
                ),
            )
            .with_method(
                self.arena.alloc(b"in_int"),
                IrFuncRef::new(
                    [IrTy::class_ty(self.io_class)],
                    IrTy::class_ty(self.int_class),
                ),
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
