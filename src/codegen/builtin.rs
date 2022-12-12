use std::borrow::Cow;

use once_cell::sync::Lazy;

use super::ctx::ty::{RegularTy, WasmTy};
use super::ctx::FuncName;

pub struct BuiltinFunc {
    pub name: FuncName<'static>,
    pub ty: WasmTy<'static>,
}

pub struct BuiltinFuncNames {
    pub string_eq: BuiltinFunc,
}

pub static BUILTIN_FUNCS: Lazy<BuiltinFuncNames> = Lazy::new(|| BuiltinFuncNames {
    string_eq: BuiltinFunc {
        name: FuncName::Plain(Cow::Borrowed("string_eq")),
        ty: WasmTy::Func {
            params: vec![RegularTy::ByteArray.into(); 2],
            ret: RegularTy::I32.into(),
        },
    },
});
