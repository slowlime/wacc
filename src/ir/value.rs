use slotmap::new_key_type;

use super::bb::Block;
use super::instr::{Instr, TermInstr};
use super::ty::IrTy;

new_key_type! {
    pub struct Value;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Param {
    pub bb: Block,
    pub idx: usize,

    /// Whether the parameter can be removed if necessary.
    ///
    /// This isn't true for function parameters, for example.
    pub removable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueKind {
    /// An instruction result.
    Instr(Instr),

    /// A value produced by a terminator instruction.
    ///
    /// Only used as a block jump argument.
    TermInstr(TermInstr),

    /// A block parameter.
    Param(Param),

    /// An alias for another value.
    Id(Value),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ValueData<'a> {
    pub ty: IrTy<'a>,
    pub kind: ValueKind,
}
