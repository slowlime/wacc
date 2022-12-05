use crate::analysis::{ClassName, DefinitionLocation, TypeCtx};
use crate::ast::ty::BuiltinClass;
use crate::util::slice_formatter;

use super::{CompleteWasmTy, TyId, TyIndex, Vtable, WasmTy};

pub fn create_ref_value_type<'buf>(ty_id: TyId, nullable: bool) -> wast::core::ValType<'static> {
    use wast::core::*;
    use wast::token::{Index, Span};

    ValType::Ref(RefType {
        nullable,
        heap: HeapType::Index(Index::Num(ty_id.index() as _, Span::from_offset(0))),
    })
}

pub fn create_storage_type<'buf>(ty_id: TyId, nullable: bool) -> wast::core::StorageType<'static> {
    wast::core::StorageType::Val(create_ref_value_type(ty_id, nullable))
}

pub fn class_span<'buf>(ty_ctx: &TypeCtx<'buf>, class_name: &ClassName<'buf>) -> wast::token::Span {
    use wast::token::Span;

    let Some(class_index) = ty_ctx.get_class(class_name) else {
        return Span::from_offset(0);
    };

    match class_index.location() {
        DefinitionLocation::UserCode(span) => Span::from_offset(span.start.byte),
        DefinitionLocation::Synthetic(_) => Span::from_offset(0),
    }
}

fn complete_class<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    ty_id: TyId,
) -> wast::core::Type<'static> {
    use wast::core::*;
    use wast::token::{Index, Span};

    // the first field stores the vtable base offset
    let mut fields = vec![StructField {
        id: None,
        mutable: false,
        ty: StorageType::Val(ValType::I32),
    }];

    let Some(&WasmTy::Class(ref class_name)) = ty_index.get_by_id(ty_id) else {
        panic!("complete_class was called for a non-class type");
    };

    let byte_array_id = ty_index
        .get_by_ty(&WasmTy::ByteArray)
        .expect("the byte array type must be defined in the type index");
    let Some(class_index) = ty_ctx.get_class(class_name) else {
        panic!("The class {} was not found in the type context", class_name);
    };

    match class_name {
        ClassName::Builtin(BuiltinClass::Int | BuiltinClass::Bool) => {
            fields.push(StructField {
                id: None,
                mutable: false,
                ty: StorageType::Val(ValType::I32),
            });
        }

        ClassName::Builtin(BuiltinClass::String) => {
            fields.push(StructField {
                id: None,
                mutable: false,
                ty: StorageType::Val(ValType::Ref(RefType {
                    nullable: false,
                    heap: HeapType::Index(Index::Num(
                        byte_array_id.index() as _,
                        Span::from_offset(0),
                    )),
                })),
            });
        }

        ClassName::Builtin(BuiltinClass::Object | BuiltinClass::IO) | ClassName::Named(_) => {
            let inheritance_chain = ty_ctx.inheritance_chain(class_name).collect::<Vec<_>>();
            let defined_fields = inheritance_chain
                .into_iter()
                .rev()
                .flat_map(|(_, index)| index.fields());

            for (field_name, _, field_ty) in defined_fields {
                let field_wasm_ty = field_ty.clone().into();
                let Some(field_ty_id) = ty_index.get_by_ty(&field_wasm_ty) else {
                    panic!("The type of the field {} of the class {} was not found in the type index",
                        slice_formatter(field_name), class_name);
                };
                fields.push(StructField {
                    id: None,
                    mutable: true,
                    ty: create_storage_type(field_ty_id, true),
                });
            }
        }

        ClassName::SelfType => unreachable!(),
    }

    let parent = class_index.parent().map(|parent_name| {
        let parent_wasm_ty = parent_name.clone().into();
        let Some(parent_ty_id) = ty_index.get_by_ty(&parent_wasm_ty) else {
            panic!("The parent class {} of the class {} was not found in the type index",
                parent_name, class_name);
        };

        Index::Num(parent_ty_id.index() as _, class_span(ty_ctx, parent_name))
    });

    Type {
        span: class_span(ty_ctx, class_name),
        id: None,
        name: None,
        def: TypeDef::Struct(StructType { fields }),
        parent,
    }
}

fn complete_func<'buf>(
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    params: &[ClassName<'buf>],
    ret: &ClassName<'buf>,
) -> wast::core::Type<'static> {
    use wast::core::*;
    use wast::token::Span;

    let param_ty_ids = params
        .into_iter()
        .map(|class_name| {
            let wasm_ty = class_name.clone().into();
            let Some(ty_id) = ty_index.get_by_ty(&wasm_ty) else {
                panic!("The type {} was not found in the type index", class_name);
            };

            create_ref_value_type(ty_id, true)
        });

    let wasm_ret_ty = ret.clone().into();
    let Some(ret_ty_id) = ty_index.get_by_ty(&wasm_ret_ty) else {
        panic!("The type {} was not found in the type index", ret);
    };

    Type {
        span: Span::from_offset(0),
        id: None,
        name: None,
        def: TypeDef::Func(FunctionType {
            params: param_ty_ids.map(|ty| (None, None, ty)).collect(),
            results: Box::new([create_ref_value_type(ret_ty_id, true)]),
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
            mutable: false,
            ty: StorageType::I8,
        }),
        parent: None,
    }
}

fn complete_wasm_ty<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    ty_id: TyId,
    wasm_ty: &WasmTy<'buf>,
) -> wast::core::Type<'static> {
    match wasm_ty {
        WasmTy::Class(_) => complete_class(ty_ctx, ty_index, ty_id),
        WasmTy::Func { params, ret } => complete_func(ty_index, params, ret),
        WasmTy::ByteArray => complete_byte_array(),
    }
}

pub fn compute_layout<'buf>(
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    vtable: &Vtable,
) -> TyIndex<'buf, CompleteWasmTy<'buf>> {
    let mut result = TyIndex::<CompleteWasmTy>::new();

    for (ty_id, wasm_ty) in ty_index.iter() {
        let complete_ty = super::layout::complete_wasm_ty(ty_ctx, ty_index, ty_id, wasm_ty);
        let vtable_base = vtable.base_offset(ty_id);
        result.insert(complete_ty, wasm_ty.clone(), vtable_base);
    }

    // XXX: need to figure out how stable the type indices are
    // In particular, we rely on the ty_ids and type indices coinciding in the compiled wasm module.
    // If it turns out they don't, we're gonna have a bad time.

    result
}