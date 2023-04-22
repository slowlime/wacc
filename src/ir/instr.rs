use slotmap::new_key_type;

use crate::util::define_byte_string;

use super::bb::BlockId;
use super::func::Func;
use super::ty::{IrClassName, IrTy};
use super::value::ValueId;

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
    pub obj: ValueId,
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
    pub func_ref: ValueId,
    pub args: Vec<ValueId>,
}

impl_into_instr!(CallRef);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call<'a> {
    pub func: Func<'a>,
    pub args: Vec<ValueId>,
}

impl_into_instr!(Call<'a>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldGet<'a> {
    pub obj: ValueId,
    pub field: FieldName<'a>,
}

impl_into_instr!(FieldGet<'a>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldSet<'a> {
    pub obj: ValueId,
    pub field: FieldName<'a>,
    pub value: ValueId,
}

impl_into_instr!(FieldSet<'a>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cast<'a> {
    pub obj: ValueId,
    pub to_ty: IrTy<'a>,
}

impl_into_instr!(Cast<'a>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstrKind<'a> {
    VTableLookup(VTableLookup<'a>),
    MethodLookup(MethodLookup<'a>),
    CallRef(CallRef),
    Call(Call<'a>),
    Id(ValueId),
    FieldGet(FieldGet<'a>),
    FieldSet(FieldSet<'a>),
    Box(ValueId),
    Unbox(ValueId),
    Cast(Cast<'a>),
    UncheckedCast(Cast<'a>),
    New(IrTy<'a>),
    IsNull(ValueId),
    Add(ValueId, ValueId),
    Sub(ValueId, ValueId),
    Mul(ValueId, ValueId),
    Div(ValueId, ValueId),
    Lt(ValueId, ValueId),
    Gt(ValueId, ValueId),
    Le(ValueId, ValueId),
    Ge(ValueId, ValueId),
    Eq(ValueId, ValueId),
    Inv(ValueId),
    Not(ValueId),
    I32(i32),
    Bytes(IrBytes<'a>),
    Bool(bool),
}

new_key_type! {
    pub struct InstrId;
}

#[derive(Debug, Clone)]
pub struct Instr<'a> {
    pub kind: InstrKind<'a>,
    bb: Option<BlockId>,
}

impl<'a> Instr<'a> {
    pub fn bb(&self) -> Option<BlockId> {
        self.bb
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockJump {
    pub bb: BlockId,
    pub args: Vec<ValueId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Branch {
    pub cond: ValueId,
    pub on_true: BlockJump,
    pub on_false: BlockJump,
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Branch(Branch),
    Jump(BlockJump),
    Return(Option<ValueId>),
}
