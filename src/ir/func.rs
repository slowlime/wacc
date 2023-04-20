use std::fmt::{self, Debug, Display};

use crate::util::slice_formatter;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncName<'a>(&'a [u8]);

impl Debug for FuncName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("FuncName")
            .field(&slice_formatter(self.0))
            .finish()
    }
}

impl Display for FuncName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", slice_formatter(self.0))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncInner<'a> {
    pub name: FuncName<'a>,
}

pub type Func<'a> = &'a FuncInner<'a>;
