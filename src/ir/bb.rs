use std::collections::HashSet;

use slotmap::new_key_type;

use super::func::Func;
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

impl Block {
    pub fn preds(self, func: &Func<'_>) -> HashSet<Block> {
        func.preds(self)
    }

    pub fn param(self, func: &Func<'_>, idx: usize) -> Option<Value> {
        func.bbs[self].params.get(idx).copied()
    }

    pub fn terminator(self, func: &Func<'_>) -> TermInstr {
        func.bbs[self].terminator
    }
}
