use std::cell::RefCell;
use std::ops::Deref;

use slotmap::{SecondaryMap, SlotMap};

use crate::util::define_byte_string;

use super::bb::{Block, BlockData};
use super::instr::{Instr, InstrKind, TermInstr, TermInstrKind};
use super::mem::ArenaRef;
use super::ty::IrTy;
use super::util::derive_ref_eq;
use super::value::{Param, Value, ValueData, ValueKind};

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

    pub fn remove_instr(&mut self, instr: Instr) {
        let bb = self.bb_instrs.get(instr);
        self.instrs.remove(instr);

        if let Some(&bb) = bb {
            let instr_idx = self.bbs[bb]
                .instrs
                .iter()
                .position(|&bb_instr| bb_instr == instr)
                .expect("the instruction present in the back-mapping must be present in the block");
            self.bbs[bb].instrs.remove(instr_idx);
        }
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
            kind: ValueKind::Param(Param {
                bb,
                idx,
                removable: true,
            }),
        });

        bb_data.params.push(value);

        value
    }

    /// Removes a param from the block and its precedessors' terminators.
    pub fn remove_param(&mut self, bb: Block, idx: usize) {
        let param = self.bbs[bb].params.remove(idx);
        let param_def = self.param_def(param).unwrap();
        assert!(param_def.removable, "the parameter cannot be removed");

        let bb_data = &mut self.bbs[bb];

        // update the indices in the shifted parameters
        for idx in param_def.idx..bb_data.params.len() {
            match self.values[bb_data.params[idx]].kind {
                ValueKind::Param(Param {
                    idx: ref mut prev_idx,
                    ..
                }) => {
                    *prev_idx = idx;
                }

                _ => unreachable!(),
            }
        }

        for &pred in &self.bb_preds[bb] {
            self.term_instrs[self.bbs[pred].terminator].remove_arg_from_jumps_to(bb, param_def.idx);
        }
    }

    pub fn add_bb(&mut self, bb_data: BlockData) -> Block {
        let term_instr = bb_data.terminator;
        let bb = self.bbs.insert(bb_data);
        let preds = self.term_instrs[term_instr]
            .jumps()
            .map(|jmp| jmp.bb)
            .collect();
        self.bb_preds.insert(bb, preds);

        bb
    }

    pub fn set_entry_bb(&mut self, bb: Block) {
        self.entry_bb = Some(bb);
    }

    pub fn unwrap_entry_bb(&self) -> Block {
        self.entry_bb.unwrap()
    }

    pub fn param_def(&self, value: Value) -> Option<Param> {
        match self.values[value].kind {
            ValueKind::Param(param) => Some(param),
            _ => None,
        }
    }

    pub fn instr_def(&self, value: Value) -> Option<Instr> {
        match self.values[value].kind {
            ValueKind::Instr(instr) => Some(instr),
            _ => None,
        }
    }

    pub fn preds(&self, bb: Block) -> Vec<Block> {
        self.bb_preds.get(bb).cloned().unwrap_or_default()
    }

    /// Resolves a chain of identity values.
    pub fn resolve_value(&self, mut value: Value) -> Value {
        while let ValueKind::Id(ref_value) = self.values[value].kind {
            value = ref_value;
        }

        value
    }

    pub fn replace_with_id(&mut self, prev_value: Value, new_value: Value) {
        let prev_value = self.resolve_value(prev_value);
        let new_value = self.resolve_value(new_value);

        match self.values[prev_value].kind {
            ValueKind::Instr(instr) => {
                self.remove_instr(instr);
            }

            ValueKind::Param(Param { bb, idx, .. }) => {
                self.remove_param(bb, idx);
            }

            // we're matching a resolved value
            ValueKind::Id(_) => unreachable!(),
        }

        self.values[prev_value].kind = ValueKind::Id(new_value);
        assert_eq!(self.values[prev_value].ty, self.values[new_value].ty);
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
