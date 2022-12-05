use crate::analysis::{ClassName, TypeCtx};
use crate::ast::ty::{BuiltinClass, FunctionTy, ResolvedTy};
use crate::util::slice_formatter;

pub const CONSTRUCTOR_NAME: &[u8] = b"{new}";

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WasmTy<'buf> {
    ByteArray,

    Class(ClassName<'buf>),

    Func {
        params: Vec<ClassName<'buf>>,
        ret: ClassName<'buf>,
    },
}

impl<'buf> From<FunctionTy<'buf>> for WasmTy<'buf> {
    fn from(FunctionTy { params, ret }: FunctionTy<'buf>) -> Self {
        Self::Func {
            params: params
                .into_iter()
                .map(|param| param.try_into().unwrap())
                .collect(),
            ret: (*ret).try_into().unwrap(),
        }
    }
}

impl<'buf> From<ClassName<'buf>> for WasmTy<'buf> {
    fn from(class_name: ClassName<'buf>) -> Self {
        assert_ne!(class_name, ClassName::SelfType);

        Self::Class(class_name)
    }
}

impl<'buf> From<ResolvedTy<'buf>> for WasmTy<'buf> {
    fn from(ty: ResolvedTy<'buf>) -> WasmTy<'buf> {
        match ty {
            ResolvedTy::Builtin(builtin) => Self::Class(builtin.into()),
            ResolvedTy::SelfType { enclosed } => (*enclosed).into(),
            ResolvedTy::Class(name) => Self::Class(name.into()),
            ResolvedTy::Function(ty) => ty.into(),

            ResolvedTy::Bottom | ResolvedTy::Untyped => {
                unreachable!("the ast must have passed typeck");
            }
        }
    }
}

pub fn constructor_ty<'buf>() -> WasmTy<'buf> {
    WasmTy::Func {
        params: vec![],
        ret: BuiltinClass::Object.into(),
    }
}

pub fn get_method_ty<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    class_name: &ClassName<'buf>,
    method_name: &[u8],
) -> WasmTy<'buf> {
    let chain = ty_ctx.inheritance_chain(class_name).collect::<Vec<_>>();
    let method = chain
        .into_iter()
        .rev()
        .find_map(|(name, index)| index.get_method_ty(method_name).map(|(_, ty)| (name, ty)));
    let Some((def_class_name, method_ty)) = method else {
        panic!("Class {} does not have method {}", class_name, slice_formatter(method_name));
    };
    let mut method_ty = method_ty.clone();
    method_ty
        .params
        .insert(0, def_class_name.clone().try_into().unwrap());

    method_ty.into()
}
