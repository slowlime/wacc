use std::fmt::{self, Debug, Display};

use crate::util::slice_formatter;

use super::func::Func;
use super::ty::{IrTy, IrClassName};
use super::value::Value;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodName<'a>(&'a [u8]);

impl Debug for MethodName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("MethodName")
            .field(&slice_formatter(&self.0))
            .finish()
    }
}

impl Display for MethodName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", slice_formatter(&self.0))
    }
}

// TODO: make a macro
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldName<'a>(&'a [u8]);

impl Debug for FieldName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("FieldName")
            .field(&slice_formatter(&self.0))
            .finish()
    }
}

impl Display for FieldName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", slice_formatter(&self.0))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VTableLookup<'a> {
    pub obj: Value<'a>,
    pub method_name: MethodName<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodLookup<'a> {
    pub class_name: IrClassName<'a>,
    pub method_name: MethodName<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallRef<'a> {
    pub func_ref: Value<'a>,
    pub args: Vec<Value<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call<'a> {
    pub func: Func<'a>,
    pub args: Vec<Value<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldGet<'a> {
    pub obj: Value<'a>,
    pub field: FieldName<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldSet<'a> {
    pub obj: Value<'a>,
    pub field: FieldName<'a>,
    pub value: Value<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cast<'a> {
    pub obj: Value<'a>,
    pub to_ty: IrTy<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstrKind<'a> {
    VTableLookup(VTableLookup<'a>),
    MethodLookup(MethodLookup<'a>),
    CallRef(CallRef<'a>),
    Call(Call<'a>),
    Id(Value<'a>),
    FieldGet(FieldGet<'a>),
    FieldSet(FieldSet<'a>),
    Box(Value<'a>),
    Unbox(Value<'a>),
    Cast(Cast<'a>),
    UncheckedCast(Cast<'a>),
    New(IrTy<'a>),
    IsNull(Value<'a>),
    Add(Value<'a>, Value<'a>),
    Sub(Value<'a>, Value<'a>),
    Mul(Value<'a>, Value<'a>),
    Div(Value<'a>, Value<'a>),
    Lt(Value<'a>, Value<'a>),
    Gt(Value<'a>, Value<'a>),
    Le(Value<'a>, Value<'a>),
    Ge(Value<'a>, Value<'a>),
    Eq(Value<'a>, Value<'a>),
    Inv(Value<'a>),
    Not(Value<'a>),
    I32(i32),
    Bytes(&'a [u8]),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Instr<'a> {
    pub kind: InstrKind<'a>,
    pub ty: Option<IrTy<'a>>,
}
