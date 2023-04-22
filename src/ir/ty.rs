use std::cell::RefCell;

use indexmap::IndexMap;

use crate::util::define_byte_string;

use super::mem::ArenaRef;
use super::util::derive_ref_eq;

define_byte_string! {
    pub struct IrClassName<'a>;
}

impl<'a> IrClassName<'a> {
    pub fn new(arena: ArenaRef<'a>, name: &[u8]) -> Self {
        Self(arena.alloc_bytes(name))
    }
}

#[derive(Debug)]
pub struct IrClassInner<'a> {
    pub name: IrClassName<'a>,
    pub parent: Option<IrClass<'a>>,
}

impl<'a> IrClassInner<'a> {
    fn new(arena: ArenaRef<'a>, name: IrClassName<'a>, parent: Option<IrClass<'a>>) -> &'a Self {
        arena.alloc_class(Self { name, parent })
    }
}

pub type IrClass<'a> = &'a IrClassInner<'a>;
derive_ref_eq!(&'a IrClassInner<'a>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IrFuncRef<'a> {
    pub args: Vec<IrTy<'a>>,
    pub ret: Box<IrTy<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IrTy<'a> {
    // unboxed types
    I32,
    Bool,
    Bytes,
    Unit,

    // boxed types
    Object(Option<IrClass<'a>>),
    FuncRef(IrFuncRef<'a>),
}

impl<'a> IrTy<'a> {
    pub fn class_ty(class: IrClass<'a>) -> IrTy<'a> {
        Self::Object(Some(class))
    }

    pub fn object_ty() -> IrTy<'a> {
        Self::Object(None)
    }

    pub fn func_ref_ty(args: Vec<IrTy<'a>>, ret: Box<IrTy<'a>>) -> IrTy<'a> {
        Self::FuncRef(IrFuncRef { args, ret })
    }

    pub fn is_boxed(&self) -> bool {
        !matches!(self, IrTy::I32 | IrTy::Bool | IrTy::Bytes | IrTy::Unit)
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
    tys: RefCell<IndexMap<IrClassName<'a>, IrClass<'a>>>,
    arena: ArenaRef<'a>,

    object_class: IrClass<'a>,
    int_class: IrClass<'a>,
    string_class: IrClass<'a>,
    bool_class: IrClass<'a>,
    io_class: IrClass<'a>,
}

impl<'a> IrTyRegistry<'a> {
    pub fn new(arena: ArenaRef<'a>) -> Self {
        let mut tys = IndexMap::new();

        let object_class = Self::add_builtin_class(arena, &mut tys, b"Object", None);
        let int_class = Self::add_builtin_class(arena, &mut tys, b"Int", Some(object_class));
        let string_class = Self::add_builtin_class(arena, &mut tys, b"String", Some(object_class));
        let bool_class = Self::add_builtin_class(arena, &mut tys, b"Bool", Some(object_class));
        let io_class = Self::add_builtin_class(arena, &mut tys, b"IO", Some(object_class));

        Self {
            tys: RefCell::new(tys),
            arena,

            object_class,
            int_class,
            string_class,
            bool_class,
            io_class,
        }
    }

    fn add_builtin_class(
        arena: ArenaRef<'a>,
        tys: &mut IndexMap<IrClassName<'a>, IrClass<'a>>,
        name: &[u8],
        parent: Option<IrClass<'a>>,
    ) -> IrClass<'a> {
        let class = IrClassInner::new(arena, IrClassName::new(arena, name), parent);
        tys.insert(class.name, class);

        class
    }

    pub fn object_class(&self) -> IrClass<'a> {
        self.object_class
    }

    pub fn int_class(&self) -> IrClass<'a> {
        self.int_class
    }

    pub fn string_class(&self) -> IrClass<'a> {
        self.string_class
    }

    pub fn bool_class(&self) -> IrClass<'a> {
        self.bool_class
    }

    pub fn io_class(&self) -> IrClass<'a> {
        self.io_class
    }
}
