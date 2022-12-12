use std::borrow::Cow;

use super::ctx::FuncName;

pub struct BuiltinFuncNames {
    pub string_eq: FuncName<'static>,
}

pub static BUILTIN_FUNCS: BuiltinFuncNames = BuiltinFuncNames {
    string_eq: FuncName::Plain(Cow::Borrowed("string_eq")),
};
