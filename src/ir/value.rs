use std::fmt::{self, Debug, Display};

use crate::util::slice_formatter;

use super::ty::IrTy;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IrBytes<'a>(&'a [u8]);

impl Debug for IrBytes<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("IrBytes")
            .field(&slice_formatter(self.0))
            .finish()
    }
}

impl Display for IrBytes<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", slice_formatter(self.0))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueKind<'a> {
    I32(i32),
    Bool(bool),
    Bytes(IrBytes<'a>),
    Null,
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueInner<'a> {
    pub static_value: ValueKind<'a>,
    pub ty: IrTy<'a>,
}

pub type Value<'a> = &'a ValueInner<'a>;
