use slotmap::new_key_type;

use super::bb::Block;
use super::instr::Instr;
use super::ty::{HasIrTy, IrTy};

new_key_type! {
    pub struct Value;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Instr(Instr),

    Param {
        bb: Block,
        idx: usize,
    },
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ValueData<'a> {
    pub ty: IrTy<'a>,
    pub kind: ValueKind,
}

impl<'a> HasIrTy<'a> for ValueData<'a> {
    fn ty(&self) -> &IrTy<'a> {
        &self.ty
    }
}
