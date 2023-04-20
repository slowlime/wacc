use super::func::FuncInner;
use super::ty::IrClassInner;
use super::value::ValueInner;

#[derive(Default)]
pub struct Arena<'a> {
    bytes: typed_arena::Arena<u8>,
    classes: typed_arena::Arena<IrClassInner<'a>>,
    values: typed_arena::Arena<ValueInner<'a>>,
    funcs: typed_arena::Arena<FuncInner<'a>>,
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

    pub fn alloc_value(&'a self, value: ValueInner<'a>) -> &'a mut ValueInner<'a> {
        self.values.alloc(value)
    }

    pub fn alloc_func(&'a self, func: FuncInner<'a>) -> &'a mut FuncInner<'a> {
        self.funcs.alloc(func)
    }
}

pub type ArenaRef<'a> = &'a Arena<'a>;
