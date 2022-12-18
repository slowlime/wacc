use std::borrow::Cow;

use crate::analysis::{ClassName, DefinitionLocation, TypeCtx};
use crate::ast::ty::BuiltinClass;
use crate::codegen::ctx::ty::RegularTy;
use crate::codegen::ctx::TyKind;
use crate::util::slice_formatter;

use super::{CompleteWasmTy, FieldTable, TyId, TyIndex, WasmTy};

pub const VTABLE_FIELD_NAME: &[u8] = b"{vtable}";
pub const VALUE_FIELD_NAME: &[u8] = b"{value}";

fn make_ref_type<'buf>(
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    ty: &WasmTy<'buf>,
    nullable: bool,
    pos: usize,
) -> wast::core::RefType<'static> {
    use wast::core::HeapType;
    use wast::core::RefType;

    match ty {
        WasmTy::Regular(RegularTy::I32) => panic!("Tried to make a reference to i32"),

        WasmTy::Regular(RegularTy::Extern) => RefType {
            nullable,
            heap: HeapType::Extern,
        },

        WasmTy::Regular(RegularTy::Class(_) | RegularTy::ByteArray) | WasmTy::Func { .. } => RefType {
            nullable,
            heap: HeapType::Index(ty_index.get_by_wasm_ty(ty).unwrap().to_wasm_index(pos)),
        },
    }
}

fn make_val_type<'buf>(
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    ty: &WasmTy<'buf>,
    nullable: bool,
    pos: usize,
) -> wast::core::ValType<'static> {
    use wast::core::ValType;

    match ty {
        WasmTy::Regular(RegularTy::I32) => ValType::I32,
        _ => ValType::Ref(make_ref_type(ty_index, ty, nullable, pos)),
    }
}

pub fn make_storage_type<'buf>(
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    ty: &WasmTy<'buf>,
    nullable: bool,
    pos: usize,
) -> wast::core::StorageType<'static> {
    wast::core::StorageType::Val(make_val_type(ty_index, ty, nullable, pos))
}

pub fn class_pos<'buf>(ty_ctx: &TypeCtx<'buf>, class_name: &ClassName<'buf>) -> usize {
    let Some(class_index) = ty_ctx.get_class(class_name) else {
        return 0;
    };

    match class_index.location() {
        DefinitionLocation::UserCode(span) => span.start.byte,
        DefinitionLocation::Synthetic(_) => 0,
    }
}

fn complete_class<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    field_table: &mut FieldTable<'buf>,
    ty_id: TyId,
) -> wast::core::Type<'static> {
    use wast::core::*;

    // the first field stores the vtable base offset
    let mut fields = vec![StructField {
        id: None,
        mutable: false,
        ty: StorageType::Val(ValType::I32),
    }];
    field_table.insert(ty_id, Cow::Borrowed(VTABLE_FIELD_NAME), TyKind::I32);

    let Some(&WasmTy::Regular(RegularTy::Class(ref class_name))) = ty_index.get_by_id(ty_id) else {
        panic!("complete_class was called for a non-class type");
    };

    let byte_array_id = ty_index
        .get_by_wasm_ty(&RegularTy::ByteArray.into())
        .expect("the byte array type must be defined in the type index");
    let Some(class_index) = ty_ctx.get_class(class_name) else {
        panic!("The class {} was not found in the type context", class_name);
    };

    match class_name {
        ClassName::Builtin(BuiltinClass::Int | BuiltinClass::Bool) => {
            field_table.insert(ty_id, Cow::Borrowed(VALUE_FIELD_NAME), TyKind::I32);
            fields.push(StructField {
                id: None,
                mutable: true,
                ty: StorageType::Val(ValType::I32),
            });
        }

        ClassName::Builtin(BuiltinClass::String) => {
            field_table.insert(ty_id, Cow::Borrowed(VALUE_FIELD_NAME), byte_array_id);
            fields.push(StructField {
                id: None,
                mutable: true,
                ty: make_storage_type(ty_index, &RegularTy::ByteArray.into(), true, 0),
            });
        }

        ClassName::Builtin(BuiltinClass::Object | BuiltinClass::IO) | ClassName::Named(_) => {
            // https://github.com/rust-lang/rust-clippy/issues/8132
            #[allow(clippy::needless_collect)]
            let inheritance_chain = ty_ctx.inheritance_chain(class_name).collect::<Vec<_>>();
            let defined_fields = inheritance_chain
                .into_iter()
                .rev()
                .flat_map(|(_, index)| index.fields());

            for (field_name, _, field_ty) in defined_fields {
                let field_wasm_ty = field_ty.clone().into();
                let Some(field_ty_id) = ty_index.get_by_wasm_ty(&field_wasm_ty) else {
                    panic!("The type of the field {} of the class {} was not found in the type index",
                        slice_formatter(field_name), class_name);
                };

                field_table.insert(ty_id, field_name.clone(), field_ty_id);
                fields.push(StructField {
                    id: None,
                    mutable: true,
                    ty: make_storage_type(ty_index, &field_wasm_ty, true, 0),
                });
            }
        }

        ClassName::SelfType => unreachable!(),
    }

    let parent = class_index.parent().map(|parent_name| {
        let parent_wasm_ty = parent_name.clone().into();
        let Some(parent_ty_id) = ty_index.get_by_wasm_ty(&parent_wasm_ty) else {
            panic!("The parent class {} of the class {} was not found in the type index",
                parent_name, class_name);
        };

        parent_ty_id.to_wasm_index(class_pos(ty_ctx, parent_name))
    });

    Type {
        span: wast::token::Span::from_offset(class_pos(ty_ctx, class_name)),
        id: None,
        name: None,
        def: TypeDef::Struct(StructType { fields }),
        parent,
    }
}

fn complete_func<'buf>(
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    params: &[RegularTy<'buf>],
    ret: Option<&RegularTy<'buf>>,
) -> wast::core::Type<'static> {
    use wast::core::*;
    use wast::token::Span;

    let param_specs: Vec<_> = params
        .iter()
        .map(|class_name| {
            let wasm_ty = class_name.clone().into();

            (None, None, make_val_type(ty_index, &wasm_ty, true, 0))
        })
        .collect();

    let ret_ty = ret.map(|ret| make_val_type(ty_index, &ret.clone().into(), true, 0));

    Type {
        span: Span::from_offset(0),
        id: None,
        name: None,
        def: TypeDef::Func(FunctionType {
            params: param_specs.into(),
            results: match ret_ty {
                Some(ret_ty) => Box::new([ret_ty]),
                None => Box::new([]),
            },
        }),
        parent: None,
    }
}

fn complete_byte_array() -> wast::core::Type<'static> {
    use wast::core::*;
    use wast::token::Span;

    wast::core::Type {
        span: Span::from_offset(0),
        id: None,
        name: None,
        def: TypeDef::Array(ArrayType {
            mutable: true,
            ty: StorageType::I8,
        }),
        parent: None,
    }
}

fn complete_wasm_ty<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    field_table: &mut FieldTable<'buf>,
    ty_id: TyId,
    wasm_ty: &WasmTy<'buf>,
) -> wast::core::Type<'static> {
    match wasm_ty {
        WasmTy::Regular(RegularTy::I32) => unreachable!(),
        WasmTy::Regular(RegularTy::Extern) => unreachable!(),
        WasmTy::Regular(RegularTy::ByteArray) => complete_byte_array(),
        WasmTy::Regular(RegularTy::Class(_)) => {
            complete_class(ty_ctx, ty_index, field_table, ty_id)
        }
        WasmTy::Func { params, ret } => complete_func(ty_index, params, ret.as_ref()),
    }
}

pub fn compute_layout<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    field_table: &mut FieldTable<'buf>,
) -> TyIndex<'buf, CompleteWasmTy<'buf>> {
    let mut result = TyIndex::<CompleteWasmTy>::new();

    for (ty_id, wasm_ty) in ty_index.iter() {
        let complete_ty =
            super::layout::complete_wasm_ty(ty_ctx, ty_index, field_table, ty_id, wasm_ty);
        result.insert(complete_ty, wasm_ty.clone());
    }

    // XXX: need to figure out how stable the type indices are
    // In particular, we rely on the ty_ids and type indices coinciding in the compiled wasm module.
    // If it turns out they don't, we're gonna have a bad time.

    result
}
