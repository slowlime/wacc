use slotmap::new_key_type;

use super::func::Func;
use super::instr::{InstrId, Terminator};
use super::value::ValueId;

new_key_type! {
    pub struct BlockId;
}

#[derive(Debug)]
pub struct Block<'a> {
    pub func: Func<'a>,
    pub instrs: Vec<InstrId>,
    pub params: Vec<ValueId>,
    pub preds: Vec<BlockId>,
    pub terminator: Terminator,
}
