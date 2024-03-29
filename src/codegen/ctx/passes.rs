//! Collects types for `TyIndex`.

use std::borrow::Cow;

use tracing::{trace, trace_span, Level};

use crate::analysis::{ClassName, TypeCtx};
use crate::codegen::funcs::{specials, SPECIAL_METHODS};
use crate::util::slice_formatter;

use super::ty::{get_method_ty, RegularTy, WasmTy, WELL_KNOWN_TYS};
use super::{MethodDefinition, MethodIndex, MethodTable, TyIndex, Vtable};

pub use super::layout::compute_layout;

pub fn collect_types<'buf>(
    sorted: &[ClassName<'buf>],
    ty_ctx: &TypeCtx<'buf>,
) -> TyIndex<'buf, 'buf, WasmTy<'buf>> {
    let span = trace_span!("collect_types");
    let _span = span.enter();

    let mut ty_index = TyIndex::<WasmTy>::new();

    for (_, ty) in WELL_KNOWN_TYS.iter() {
        let ty_id = ty_index.insert(ty.clone());
        trace!(?ty_id, ?ty, "Added a well-known type");
    }

    for name in sorted {
        let ty: WasmTy = RegularTy::Class(name.clone()).into();

        if tracing::enabled!(Level::TRACE) {
            let ty_id = ty_index.insert(ty.clone());
            trace!(?ty_id, ty = ?ty, "Added a class type");
        } else {
            ty_index.insert(ty);
        }
    }

    for (key, func) in specials() {
        let ty_id = ty_index.insert(func.ty.clone());
        trace!(?ty_id, ty = ?&func.ty, "Added a functional type for a special function {:?}", key);
    }

    for (key, func) in SPECIAL_METHODS.iter() {
        let ty_id = ty_index.insert(func.ty.clone());
        trace!(?ty_id, ty = ?&func.ty, "Added a functional type for a special method {:?}", key);
    }

    for name in sorted {
        let class_span = trace_span!("class method", class_name = %name);
        let _class_span = class_span.enter();

        let Some(class_index) = ty_ctx.get_class(name) else {
            panic!("The class {} was not found in the type context", name);
        };

        for (method_name, _, _) in class_index.methods() {
            let method_ty = get_method_ty(ty_ctx, name, method_name);

            if tracing::enabled!(Level::TRACE) {
                let ty_id = ty_index.insert(method_ty.clone());
                trace!(
                    ?ty_id, ty = ?&method_ty,
                    "Added a functional type for a class method `{}`", slice_formatter(method_name)
                );
            } else {
                ty_index.insert(method_ty.clone());
            }
        }
    }

    ty_index
}

pub fn enumerate_methods<'buf>(
    sorted: &[ClassName<'buf>],
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, 'buf, WasmTy<'buf>>,
) -> MethodIndex<'buf> {
    let span = trace_span!("enumerate_methods");
    let _span = span.enter();

    let mut method_index = MethodIndex::new();

    for class in sorted {
        let class_span = trace_span!("class processing", class_name = %&class);
        let _class_span = class_span.enter();

        let wasm_ty = class.clone().into();
        let Some(ty_id) = ty_index.get_by_wasm_ty(&wasm_ty) else {
            panic!("The type of the class {} is not present in the type index", class);
        };
        let class_index = ty_ctx
            .get_class(class)
            .expect("all types must be present in the type context");

        if let Some(parent) = class_index.parent() {
            let super_wasm_ty = parent.clone().into();
            let Some(super_ty_id) = ty_index.get_by_wasm_ty(&super_wasm_ty) else {
                panic!("While processing the class {}, the type of its superclass {} was not found in the type index",
                    class, parent);
            };
            method_index.inherit(super_ty_id, ty_id);
        }

        for (key, func) in SPECIAL_METHODS.iter() {
            let method_ty_id = ty_index.get_by_wasm_ty(&func.ty).unwrap();
            let method_id = method_index.insert(ty_id, Cow::Borrowed(func.name), method_ty_id);

            trace!(?key, ?method_ty_id, ?method_id, "Inserted a special method");
        }

        for (method_name, _, _) in class_index.methods() {
            let wasm_method_ty = get_method_ty(ty_ctx, class, method_name);
            let Some(method_ty_id) = ty_index.get_by_wasm_ty(&wasm_method_ty) else {
                panic!("While processing the class {}, the type of the method {} was not found in the type index",
                    class, slice_formatter(method_name));
            };

            let method_id = method_index.insert(ty_id, method_name.clone(), method_ty_id);
            trace!(method_name = %slice_formatter(method_name), ?method_ty_id, ?method_id, "Inserted a class method");
        }
    }

    method_index
}

pub fn create_method_table<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, 'buf, WasmTy<'buf>>,
    method_index: &MethodIndex<'buf>,
) -> MethodTable {
    let mut method_table = MethodTable::new();

    for (class_name, class_index) in ty_ctx.iter() {
        let wasm_ty = class_name.clone().into();
        let Some(ty_id) = ty_index.get_by_wasm_ty(&wasm_ty) else {
            panic!("The class {} was not found in the type index", class_name);
        };

        for (_, func) in SPECIAL_METHODS.iter() {
            let method_id = method_index.get_by_name(ty_id, func.name).unwrap();
            let method_ty_id = ty_index.get_by_wasm_ty(&func.ty).unwrap();
            method_table.insert(method_ty_id, method_id);
        }

        for (method_name, _, _) in class_index.methods() {
            let Some(method_id) = method_index.get_by_name(ty_id, method_name) else {
                panic!("The method {} of the class {} was not found in the method index",
                    slice_formatter(method_name), class_name);
            };
            let &MethodDefinition {
                method_ty_id,
                last_def_id,
                ..
            } = method_index.get_by_id(method_id).unwrap().1;
            assert_eq!(last_def_id, method_id);
            method_table.insert(method_ty_id, method_id);
        }
    }

    method_table
}

pub fn create_vtable<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, 'buf, WasmTy<'buf>>,
    method_index: &MethodIndex<'buf>,
    method_table: &MethodTable,
) -> Vtable {
    let span = trace_span!("create_vtable");
    let _span = span.enter();

    let mut vtable = Vtable::new();

    for (class_name, _) in ty_ctx.iter() {
        let class_span = trace_span!("process_class", %class_name);
        let _class_span = class_span.enter();

        let wasm_ty = class_name.clone().into();
        let Some(ty_id) = ty_index.get_by_wasm_ty(&wasm_ty) else {
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

            trace!(
                ?table_id,
                "Pushing the method {}",
                slice_formatter(method_name)
            );
        }

        let method_count = method_table_ids.len();
        let base_offset = vtable.insert(ty_id, method_table_ids);
        trace!(?base_offset, "Added {} methods to the vtable", method_count);
    }

    vtable
}
