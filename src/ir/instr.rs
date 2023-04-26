use slotmap::new_key_type;

use crate::util::define_byte_string;

use super::bb::Block;
use super::func::Func;
use super::ty::{IrClassName, IrTy};
use super::value::Value;

macro_rules! impl_into_instr {
    ($struct:ident) => {
        impl<'a> From<$struct> for InstrKind<'a> {
            fn from(instr: $struct) -> Self {
                Self::$struct(instr)
            }
        }
    };

    ($struct:ident<'a>) => {
        impl<'a> From<$struct<'a>> for InstrKind<'a> {
            fn from(instr: $struct<'a>) -> Self {
                Self::$struct(instr)
            }
        }
    };
}

define_byte_string! {
    pub struct MethodName<'a>;
    pub struct FieldName<'a>;
    pub struct IrBytes<'a>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VTableLookup<'a> {
    pub obj: Value,
    pub method_name: MethodName<'a>,
}

impl_into_instr!(VTableLookup<'a>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodLookup<'a> {
    pub class_name: IrClassName<'a>,
    pub method_name: MethodName<'a>,
}

impl_into_instr!(MethodLookup<'a>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallRef {
    pub func_ref: Value,
    pub args: Vec<Value>,
}

impl_into_instr!(CallRef);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call<'a> {
    pub func: Func<'a>,
    pub args: Vec<Value>,
}

impl_into_instr!(Call<'a>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldGet<'a> {
    pub obj: Value,
    pub field: FieldName<'a>,
}

impl_into_instr!(FieldGet<'a>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldSet<'a> {
    pub obj: Value,
    pub field: FieldName<'a>,
    pub value: Value,
}

impl_into_instr!(FieldSet<'a>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cast<'a> {
    pub obj: Value,
    pub to_ty: IrTy<'a>,
}

impl_into_instr!(Cast<'a>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstrKind<'a> {
    VTableLookup(VTableLookup<'a>),
    MethodLookup(MethodLookup<'a>),
    CallRef(CallRef),
    Call(Call<'a>),
    FieldGet(FieldGet<'a>),
    FieldSet(FieldSet<'a>),
    Box(Value),
    Unbox(Value),
    Cast(Cast<'a>),
    UncheckedCast(Cast<'a>),
    New(IrTy<'a>),
    IsNull(Value),
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Lt(Value, Value),
    Gt(Value, Value),
    Le(Value, Value),
    Ge(Value, Value),
    Eq(Value, Value),
    Inv(Value),
    Not(Value),
    I32(i32),
    Bytes(IrBytes<'a>),
    Bool(bool),
}

new_key_type! {
    pub struct Instr;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockJump {
    pub bb: Block,
    pub args: Vec<Value>,
}

impl BlockJump {
    pub fn remove_arg_from_jumps_to(&mut self, bb: Block, idx: usize) {
        if self.bb == bb {
            self.args.remove(idx);
        }
    }

    pub fn add_arg_to_jumps_to(&mut self, bb: Block, idx: usize, value: Value) {
        if self.bb == bb {
            self.args.insert(idx, value);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Branch {
    pub cond: Value,
    pub on_true: BlockJump,
    pub on_false: BlockJump,
}

#[derive(Debug, Clone)]
pub enum TermInstrKind {
    Branch(Branch),
    Jump(BlockJump),
    Return(Option<Value>),
    /// The block never reaches the end.
    Diverge,
}

impl TermInstrKind {
    pub fn remove_arg_from_jumps_to(&mut self, bb: Block, idx: usize) {
        match self {
            Self::Branch(Branch { on_true, on_false, .. }) => {
                on_true.remove_arg_from_jumps_to(bb, idx);
                on_false.remove_arg_from_jumps_to(bb, idx);
            }

            Self::Jump(jmp) => jmp.remove_arg_from_jumps_to(bb, idx),

            Self::Return(_) => {},
            Self::Diverge => {},
        }
    }

    // TODO: deduplicate this...
    pub fn add_arg_to_jumps_to(&mut self, bb: Block, idx: usize, value: Value) {
        match self {
            Self::Branch(Branch { on_true, on_false, .. }) => {
                on_true.add_arg_to_jumps_to(bb, idx, value);
                on_false.add_arg_to_jumps_to(bb, idx, value);
            }

            Self::Jump(jmp) => jmp.add_arg_to_jumps_to(bb, idx, value),

            Self::Return(_) => {},
            Self::Diverge => {},
        }
    }

    pub fn jumps(&self) -> Jumps<'_> {
        Jumps {
            term_instr: self,
            idx: 0,
        }
    }
}

new_key_type! {
    pub struct TermInstr;
}

pub struct Jumps<'d> {
    term_instr: &'d TermInstrKind,
    idx: usize,
}

impl<'d> Iterator for Jumps<'d> {
    type Item = &'d BlockJump;

    fn next(&mut self) -> Option<Self::Item> {
        let result = match self.term_instr {
            TermInstrKind::Branch(Branch { on_true, .. }) if self.idx == 0 => Some(on_true),
            TermInstrKind::Branch(Branch { on_false, .. }) if self.idx == 1 => Some(on_false),
            TermInstrKind::Branch(_) => None,

            TermInstrKind::Jump(jmp) if self.idx == 0 => Some(jmp),
            TermInstrKind::Jump(_) => None,

            TermInstrKind::Return(_) => None,

            TermInstrKind::Diverge => None,
        };

        if result.is_some() {
            self.idx += 1;
        }

        result
    }
}
