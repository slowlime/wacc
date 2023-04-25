use super::func::{FuncData, Func, FuncCell};
use super::ty::IrClassInner;

#[derive(Default)]
pub struct Arena<'a> {
    bytes: typed_arena::Arena<u8>,
    classes: typed_arena::Arena<IrClassInner<'a>>,
    funcs: typed_arena::Arena<FuncCell<'a>>,
}

impl<'a> Arena<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_bytes(&'a self, bytes: &[u8]) -> &'a [u8] {
        self.bytes.alloc_extend(bytes.iter().copied())
    }

    pub fn alloc_class(&'a self, class: IrClassInner<'a>) -> &'a mut IrClassInner<'a> {
        self.classes.alloc(class)
    }

    pub fn alloc_func(&'a self, func: FuncData<'a>) -> Func<'a> {
        self.funcs.alloc(FuncCell::new(func))
    }
}

pub type ArenaRef<'a> = &'a Arena<'a>;
