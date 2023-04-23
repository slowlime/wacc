use super::func::FuncData;
use super::ty::IrClassInner;

#[derive(Default)]
pub struct Arena<'a> {
    bytes: typed_arena::Arena<u8>,
    classes: typed_arena::Arena<IrClassInner<'a>>,
    funcs: typed_arena::Arena<FuncData<'a>>,
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

    pub fn alloc_func(&'a self, func: FuncData<'a>) -> &'a mut FuncData<'a> {
        self.funcs.alloc(func)
    }
}

pub type ArenaRef<'a> = &'a Arena<'a>;
