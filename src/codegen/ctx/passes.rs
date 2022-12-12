//! Collects types for `TyIndex`.

use std::borrow::Cow;

use crate::analysis::{ClassName, TypeCtx};
use crate::codegen::funcs::specials;
use crate::util::slice_formatter;

use super::ty::{
    constructor_ty, get_method_ty, initializer_ty, RegularTy, WasmTy, CONSTRUCTOR_NAME,
    INITIALIZER_NAME,
};
use super::{MethodDefinition, MethodIndex, MethodTable, TyIndex, Vtable};

pub use super::layout::compute_layout;

pub fn collect_types<'buf>(
    sorted: &[ClassName<'buf>],
    ty_ctx: &TypeCtx<'buf>,
) -> TyIndex<'buf, WasmTy<'buf>> {
    let mut ty_index = TyIndex::<WasmTy>::new();

    ty_index.insert(RegularTy::ByteArray.into());

    for name in sorted {
        ty_index.insert(RegularTy::Class(name.clone()).into());
    }

    ty_index.insert(constructor_ty());
    ty_index.insert(initializer_ty());

    for (_, func) in specials() {
        ty_index.insert(func.ty.clone());
    }

    for name in sorted {
        let Some(class_index) = ty_ctx.get_class(name) else {
            panic!("The class {} was not found in the type context", name);
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
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
) -> MethodIndex<'buf> {
    let mut method_index = MethodIndex::new();

    for class in sorted {
        let wasm_ty = class.clone().into();
        let Some(ty_id) = ty_index.get_by_ty(&wasm_ty) else {
            panic!("The type of the class {} is not present in the type index", class);
        };
        let class_index = ty_ctx
            .get_class(class)
            .expect("all types must be present in the type context");

        if let Some(parent) = class_index.parent() {
            let super_wasm_ty = parent.clone().into();
            let Some(super_ty_id) = ty_index.get_by_ty(&super_wasm_ty) else {
                panic!("While processing the class {}, the type of its superclass {} was not found in the type index",
                    class, parent);
            };
            method_index.inherit(super_ty_id, ty_id);
        }

        // The first method must be the constructor — this invariant is relied upon in codegen
        let Some(constructor_ty_id) = ty_index.get_by_ty(&constructor_ty()) else {
            panic!("While processing the class {}, the constructor type was not found in the type index",
                class);
        };
        method_index.insert(
            ty_id,
            Cow::Owned(CONSTRUCTOR_NAME.to_vec()),
            constructor_ty_id,
        );

        // The second method must be the initializer
        let Some(initializer_ty_id) = ty_index.get_by_ty(&initializer_ty()) else {
            panic!("While processing the class {}, the initializer type was not found in the type index",
                class);
        };
        method_index.insert(
            ty_id,
            Cow::Owned(INITIALIZER_NAME.to_vec()),
            initializer_ty_id,
        );

        for (method_name, _, _) in class_index.methods() {
            let wasm_method_ty = get_method_ty(ty_ctx, class, method_name);
            let Some(method_ty_id) = ty_index.get_by_ty(&wasm_method_ty) else {
                panic!("While processing the class {}, the type of the method {} was not found in the type index",
                    class, slice_formatter(method_name));
            };
            method_index.insert(ty_id, method_name.clone(), method_ty_id);
        }
    }

    method_index
}

pub fn create_method_table<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    method_index: &MethodIndex<'buf>,
) -> MethodTable {
    let mut method_table = MethodTable::new();

    for (class_name, class_index) in ty_ctx.iter() {
        let wasm_ty = class_name.clone().into();
        let Some(ty_id) = ty_index.get_by_ty(&wasm_ty) else {
            panic!("The class {} was not found in the type index", class_name);
        };

        let Some(constructor_method_id) = method_index.get_by_name(ty_id, CONSTRUCTOR_NAME) else {
            panic!("The constructor of the class {} is not present in the method index", class_name);
        };
        method_table.insert(ty_id, constructor_method_id);

        let Some(initializer_method_id) = method_index.get_by_name(ty_id, INITIALIZER_NAME) else {
            panic!("The initializer of the class {} is not present in the method index", class_name)
        };
        method_table.insert(ty_id, initializer_method_id);

        for (method_name, _, _) in class_index.methods() {
            let Some(method_id) = method_index.get_by_name(ty_id, &method_name) else {
                panic!("The method {} of the class {} was not found in the method index",
                    slice_formatter(method_name), class_name);
            };
            let &MethodDefinition { last_def_id, .. } =
                method_index.get_by_id(method_id).unwrap().1;
            assert_eq!(last_def_id, method_id);

            method_table.insert(ty_id, method_id);
        }
    }

    method_table
}

pub fn create_vtable<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    method_index: &MethodIndex<'buf>,
    method_table: &MethodTable,
) -> Vtable {
    let mut vtable = Vtable::new();

    for (class_name, _) in ty_ctx.iter() {
        let wasm_ty = class_name.clone().into();
        let Some(ty_id) = ty_index.get_by_ty(&wasm_ty) else {
            panic!("The class {} was not found in the type index", class_name);
        };
        let Some(methods) = method_index.methods(ty_id) else {
            panic!("The class {} has no methods registered in the method index", class_name);
        };

        let mut method_table_ids = vec![];

        for (method_name, def) in methods {
            let &MethodDefinition {
                method_ty_id,
                first_def_id: _,
                last_def_id,
            } = def;
            let Some(table_id) = method_table.get_by_method_id(method_ty_id, last_def_id) else {
                panic!("The last definition of the method {} of the class {} was not found in the method table",
                    slice_formatter(method_name), class_name);
            };
            method_table_ids.push(table_id);
        }

        vtable.insert(ty_id, method_table_ids);
    }

    vtable
}
