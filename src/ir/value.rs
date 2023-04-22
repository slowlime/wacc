use slotmap::new_key_type;

use super::bb::BlockId;
use super::instr::InstrId;
use super::ty::{HasIrTy, IrTy};

new_key_type! {
    pub struct ValueId;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Instr(InstrId),

    Param {
        bb: BlockId,
        idx: usize,
    },
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Value<'a> {
    pub ty: IrTy<'a>,
    pub kind: ValueKind,
}

impl<'a> HasIrTy<'a> for Value<'a> {
    fn ty(&self) -> &IrTy<'a> {
        &self.ty
    }
}
