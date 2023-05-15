use std::collections::HashSet;
use std::fmt::{self, Display};
use std::num::NonZeroUsize;
use std::ops::{Index, IndexMut};

use indexmap::IndexMap;
use serde::Serialize;
use slotmap::{SecondaryMap, SlotMap};

use crate::ir::instr::InstrOperands;
use crate::try_match;
use crate::util::define_byte_string;

use super::bb::{Block, BlockData};
use super::instr::{Instr, InstrKind, MethodName, TermInstr, TermInstrKind};
use super::mem::ArenaRef;
use super::ty::{IrClassName, IrTy};
use super::value::{Param, Value, ValueData, ValueKind};

define_byte_string! {
    pub struct FuncName<'a>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FullFuncName<'a> {
    Free(FuncName<'a>),
    Method(IrClassName<'a>, MethodName<'a>),
}

impl<'a> From<FuncName<'a>> for FullFuncName<'a> {
    fn from(name: FuncName<'a>) -> Self {
        Self::Free(name)
    }
}

impl Display for FullFuncName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Free(func) => func.fmt(f),
            Self::Method(class, func) => write!(f, "{}::{}", class, func),
        }
    }
}

#[derive(Debug)]
pub struct Func<'a> {
    pub id: FuncId,
    pub name: FullFuncName<'a>,
    pub bbs: SlotMap<Block, BlockData>,
    pub bb_preds: SecondaryMap<Block, HashSet<Block>>,
    pub instrs: SlotMap<Instr, InstrKind<'a>>,
    pub bb_instrs: SecondaryMap<Instr, Block>,
    pub term_instrs: SlotMap<TermInstr, TermInstrKind<'a>>,
    pub values: SlotMap<Value, ValueData<'a>>,
    entry_bb: Option<Block>,
}

impl<'a> Func<'a> {
    pub fn append_instr(&mut self, bb: Block, instr_kind: InstrKind<'a>, ty: IrTy<'a>) -> Value {
        let instr = self.instrs.insert(instr_kind);
        let value_data = ValueData {
            ty,
            kind: ValueKind::Instr(instr),
        };

        self.bbs[bb].instrs.push(instr);
        self.bb_instrs[instr] = bb;

        self.values.insert(value_data)
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

    pub fn add_term_instr(&mut self, instr: TermInstrKind<'a>) -> TermInstr {
        self.term_instrs.insert(instr)
    }

    pub fn make_term_instr_value(&mut self, term_instr: TermInstr, ty: IrTy<'a>) -> Value {
        let value_data = ValueData {
            ty,
            kind: ValueKind::TermInstr(term_instr),
        };

        self.values.insert(value_data)
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
            for jmp in self.term_instrs[self.bbs[pred].terminator].jumps_to_mut(bb) {
                jmp.args.remove(param_def.idx);
            }
        }
    }

    pub fn add_diverging_bb(&mut self) -> Block {
        let term_instr = self.add_term_instr(TermInstrKind::Diverge);
        let bb_data = BlockData::new(term_instr);

        self.add_bb(bb_data)
    }

    pub fn add_bb(&mut self, bb_data: BlockData) -> Block {
        let term_instr = bb_data.terminator;
        let bb = self.bbs.insert(bb_data);
        let preds = self.term_instrs[term_instr]
            .operands()
            .iter()
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

    pub fn preds(&self, bb: Block) -> HashSet<Block> {
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

            ValueKind::TermInstr(_) => {
                panic!("cannot optimize away a terminator instruction result");
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

    pub fn terminate_block(&mut self, bb: Block, terminator: TermInstr) {
        let prev_succs = self.term_instrs[self.bbs[bb].terminator].succ_bbs();

        for succ in prev_succs {
            self.bb_preds[succ].remove(&bb);
        }

        self.bbs[bb].terminator = terminator;

        for succ in self.term_instrs[terminator].succ_bbs() {
            self.bb_preds[succ].insert(bb);
        }
    }
}

#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FuncId(NonZeroUsize);

impl FuncId {
    pub fn idx(&self) -> usize {
        self.0.get() - 1
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExternalFunc<'a> {
    pub id: FuncId,
    pub name: FuncName<'a>,
}

#[derive(Debug)]
pub enum FuncDef<'a> {
    Local(Func<'a>),
    External(ExternalFunc<'a>),
}

impl<'a> FuncDef<'a> {
    pub fn local(&self) -> Option<&Func<'a>> {
        try_match!(self, Self::Local(func) => func)
    }

    pub fn local_mut(&mut self) -> Option<&mut Func<'a>> {
        try_match!(self, Self::Local(func) => func)
    }

    pub fn external(&self) -> Option<&ExternalFunc<'a>> {
        try_match!(self, Self::External(func) => func)
    }

    pub fn external_mut(&mut self) -> Option<&mut ExternalFunc<'a>> {
        try_match!(self, Self::External(func) => func)
    }
}

#[derive(Debug)]
pub struct FuncRegistry<'a> {
    funcs: IndexMap<FullFuncName<'a>, FuncDef<'a>>,

    pub abort_func: FuncId,
    pub print_bytes_func: FuncId,
    pub print_int_func: FuncId,
    pub read_line_func: FuncId,
    pub read_int_func: FuncId,
}

impl<'a> FuncRegistry<'a> {
    pub fn new(arena: ArenaRef<'a>) -> Self {
        let mut this = Self {
            funcs: Default::default(),

            abort_func: FuncId(NonZeroUsize::new(1).unwrap()),
            print_bytes_func: FuncId(NonZeroUsize::new(1).unwrap()),
            print_int_func: FuncId(NonZeroUsize::new(1).unwrap()),
            read_line_func: FuncId(NonZeroUsize::new(1).unwrap()),
            read_int_func: FuncId(NonZeroUsize::new(1).unwrap()),
        };

        this.abort_func = this.add_external_func(arena.alloc(b"abort")).id;
        this.print_bytes_func = this.add_external_func(arena.alloc(b"print_bytes")).id;
        this.print_int_func = this.add_external_func(arena.alloc(b"print_int")).id;
        this.read_line_func = this.add_external_func(arena.alloc(b"read_line")).id;
        this.read_int_func = this.add_external_func(arena.alloc(b"read_int")).id;

        this
    }

    pub fn add_local_func<'r>(&'r mut self, name: FullFuncName<'a>) -> &'r mut Func<'a> {
        self.add_func_def(name, |id| {
            FuncDef::Local(Func {
                id,
                name,
                bbs: Default::default(),
                bb_preds: Default::default(),
                instrs: Default::default(),
                bb_instrs: Default::default(),
                term_instrs: Default::default(),
                values: Default::default(),
                entry_bb: Default::default(),
            })
        })
        .local_mut()
        .unwrap()
    }

    pub fn add_external_func<'r>(&'r mut self, name: FuncName<'a>) -> &'r mut ExternalFunc<'a> {
        self.add_func_def(name.into(), |id| {
            FuncDef::External(ExternalFunc { id, name })
        })
        .external_mut()
        .unwrap()
    }

    fn add_func_def<'r>(
        &'r mut self,
        name: FullFuncName<'a>,
        func_def: impl FnOnce(FuncId) -> FuncDef<'a>,
    ) -> &'r mut FuncDef<'a> {
        use indexmap::map::Entry;

        let id = FuncId(NonZeroUsize::new(self.funcs.len() + 1).unwrap());

        match self.funcs.entry(name) {
            Entry::Vacant(entry) => entry.insert(func_def(id)),

            Entry::Occupied(_) => {
                panic!("Function {} is defined twice", name);
            }
        }
    }
}

impl<'a> Index<FuncId> for FuncRegistry<'a> {
    type Output = FuncDef<'a>;

    fn index(&self, index: FuncId) -> &Self::Output {
        &self.funcs[index.idx()]
    }
}

impl<'a> IndexMut<FuncId> for FuncRegistry<'a> {
    fn index_mut(&mut self, index: FuncId) -> &mut Self::Output {
        &mut self.funcs[index.idx()]
    }
}

impl<'a> Index<FullFuncName<'a>> for FuncRegistry<'a> {
    type Output = FuncDef<'a>;

    fn index(&self, index: FullFuncName<'a>) -> &Self::Output {
        &self.funcs[&index]
    }
}

impl<'a> IndexMut<FullFuncName<'a>> for FuncRegistry<'a> {
    fn index_mut(&mut self, index: FullFuncName<'a>) -> &mut Self::Output {
        &mut self.funcs[&index]
    }
}

impl<'a> Index<FuncName<'a>> for FuncRegistry<'a> {
    type Output = FuncDef<'a>;

    fn index(&self, index: FuncName<'a>) -> &Self::Output {
        &self.funcs[&FullFuncName::from(index)]
    }
}

impl<'a> IndexMut<FuncName<'a>> for FuncRegistry<'a> {
    fn index_mut(&mut self, index: FuncName<'a>) -> &mut Self::Output {
        &mut self.funcs[&FullFuncName::from(index)]
    }
}
