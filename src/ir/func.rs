use std::cell::RefCell;
use std::ops::Deref;

use slotmap::{SecondaryMap, SlotMap};

use crate::util::define_byte_string;

use super::bb::{Block, BlockData};
use super::instr::{Instr, InstrKind, TermInstr, TermInstrKind};
use super::mem::ArenaRef;
use super::ty::IrTy;
use super::util::derive_ref_eq;
use super::value::{Value, ValueData, ValueKind};

define_byte_string! {
    pub struct FuncName<'a>;
}

impl<'a> FuncName<'a> {
    pub fn new(arena: ArenaRef<'a>, name: &[u8]) -> Self {
        Self(arena.alloc_bytes(name))
    }
}

#[derive(Debug)]
pub struct FuncData<'a> {
    pub name: FuncName<'a>,
    pub bbs: SlotMap<Block, BlockData>,
    pub bb_preds: SecondaryMap<Block, Vec<Block>>,
    pub instrs: SlotMap<Instr, InstrKind<'a>>,
    pub bb_instrs: SecondaryMap<Instr, Block>,
    pub term_instrs: SlotMap<TermInstr, TermInstrKind>,
    pub values: SlotMap<Value, ValueData<'a>>,
    entry_bb: Option<Block>,
}

impl<'a> FuncData<'a> {
    pub fn new(name: FuncName<'a>) -> Self {
        Self {
            name,
            bbs: Default::default(),
            bb_preds: Default::default(),
            instrs: Default::default(),
            bb_instrs: Default::default(),
            term_instrs: Default::default(),
            values: Default::default(),
            entry_bb: None,
        }
    }

    pub fn add_instr(&mut self, instr_kind: InstrKind<'a>, ty: IrTy<'a>) -> Value {
        let instr = self.instrs.insert(instr_kind);
        let value = ValueData {
            ty,
            kind: ValueKind::Instr(instr),
        };

        self.values.insert(value)
    }

    pub fn add_term_instr(&mut self, instr: TermInstrKind) -> TermInstr {
        self.term_instrs.insert(instr)
    }

    // This does not modify jumps to the bb.
    pub fn append_param(&mut self, bb: Block, ty: IrTy<'a>) -> Value {
        let bb_data = &mut self.bbs[bb];
        let idx = bb_data.params.len();
        let value = self.values.insert(ValueData {
            ty,
            kind: ValueKind::Param { bb, idx },
        });

        bb_data.params.push(value);

        value
    }

    /// Removes a param from the block and its precedessors' terminators.
    pub fn swap_remove_param(&mut self, bb: Block, idx: usize) {
        let bb_data = &mut self.bbs[bb];
        bb_data.params.swap_remove(idx);

        let prev_idx = match self.values[bb_data.params[idx]].kind {
            ValueKind::Param {
                idx: ref mut prev_idx,
                ..
            } => {
                *prev_idx = idx;
                *prev_idx
            }

            _ => unreachable!(),
        };

        for &pred in &self.bb_preds[bb] {
            self.term_instrs[self.bbs[pred].terminator]
                .swap_remove_arg_from_jumps_to(bb, prev_idx);
        }
    }

    pub fn add_bb(&mut self, bb_data: BlockData) -> Block {
        let term_instr = bb_data.terminator;
        let bb = self.bbs.insert(bb_data);
        let preds = self.term_instrs[term_instr].jumps().map(|jmp| jmp.bb).collect();
        self.bb_preds.insert(bb, preds);

        bb
    }

    pub fn set_entry_bb(&mut self, bb: Block) {
        self.entry_bb = Some(bb);
    }

    pub fn unwrap_entry_bb(&self) -> Block {
        self.entry_bb.unwrap()
    }
}

#[derive(Debug)]
pub struct FuncCell<'a>(RefCell<FuncData<'a>>);

impl<'a> FuncCell<'a> {
    pub fn new(func_data: FuncData<'a>) -> Self {
        Self(RefCell::new(func_data))
    }
}

impl<'a> Deref for FuncCell<'a> {
    type Target = RefCell<FuncData<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub type Func<'a> = &'a FuncCell<'a>;
derive_ref_eq!(&'a FuncCell<'a>);
