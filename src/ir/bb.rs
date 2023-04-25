use slotmap::new_key_type;

use super::instr::{Instr, TermInstr};
use super::value::Value;

new_key_type! {
    pub struct Block;
}

#[derive(Debug)]
pub struct BlockData {
    pub instrs: Vec<Instr>,
    pub params: Vec<Value>,
    pub terminator: TermInstr,
}

impl BlockData {
    pub fn new(terminator: TermInstr) -> Self {
        Self {
            instrs: vec![],
            params: vec![],
            terminator,
        }
    }
}
