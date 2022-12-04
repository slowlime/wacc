//! Collects types for `TyIndex`.

use std::borrow::Cow;

use crate::analysis::{ClassName, TypeCtx};
use crate::ast::ty::{BuiltinClass, ResolvedTy};
use crate::util::slice_formatter;

use super::{MethodIndex, TyIndex, WasmTy};

pub const CONSTRUCTOR_NAME: &[u8] = b"{new}";

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
    method_ty.params.insert(0, def_class_name.clone().try_into().unwrap());

    method_ty.into()
}

pub fn collect_types<'buf>(
    sorted: &[ClassName<'buf>],
    ty_ctx: &TypeCtx<'buf>,
) -> TyIndex<'buf, WasmTy<'buf>> {
    let mut ty_index = TyIndex::new();

    for name in sorted {
        ty_index.insert(WasmTy::Class(name.clone()));
    }

    ty_index.insert(constructor_ty());

    for name in sorted {
        let Some(class_index) = ty_ctx.get_class(name) else {
            panic!("Class {} was not found in the type context", name);
        };

        for (method_name, _, _) in class_index.methods() {
            let method_ty = get_method_ty(ty_ctx, name, &method_name);
            ty_index.insert(method_ty);
        }
    }

    ty_index
}

pub fn enumerate_methods<'buf>(
    sorted: &[ClassName<'buf>],
    ty_ctx: &TypeCtx<'buf>,
    ty_index: TyIndex<'buf, WasmTy<'buf>>,
) -> MethodIndex<'buf> {
    let mut method_index = MethodIndex::new();

    for class in sorted {
        let wasm_ty = class.clone().into();
        let Some(ty_id) = ty_index.get_by_ty(&wasm_ty) else {
            panic!("The type of class {} is not present in the type index", class);
        };
        let class_index = ty_ctx
            .get_class(class)
            .expect("all types must be present in the type context");

        if let Some(parent) = class_index.parent() {
            let super_wasm_ty = parent.clone().into();
            let Some(super_ty_id) = ty_index.get_by_ty(&super_wasm_ty) else {
                panic!("While processing class {}, the type of its superclass {} was not found in the type index",
                    class, parent);
            };
            method_index.inherit(super_ty_id, ty_id);
        }

        // The first method must be the constructor — this invariant is relied upon in codegen
        let Some(constructor_ty_id) = ty_index.get_by_ty(&constructor_ty()) else {
            panic!("While processing class {}, the constructor type was not found in the type index",
                class);
        };
        method_index.insert(
            ty_id,
            Cow::Owned(CONSTRUCTOR_NAME.to_vec()),
            constructor_ty_id,
        );

        for (method_name, _, _) in class_index.methods() {
            let wasm_method_ty = get_method_ty(ty_ctx, class, method_name);
            let Some(method_ty_id) = ty_index.get_by_ty(&wasm_method_ty) else {
                panic!("While processing class {}, the type of the method {} was not found in the type index",
                    class, slice_formatter(method_name));
            };
            method_index.insert(ty_id, method_name.clone(), method_ty_id);
        }
    }

    method_index
}
