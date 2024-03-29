use crate::analysis::TypeCtx;
use crate::ast::Class;

use super::ctx::{
    CompleteWasmTy, FieldTable, MethodIndex, MethodTable, StringTable, TyIndex, Vtable,
};
use super::string_collector::StringCollector;
use super::{Codegen, CodegenOutput, AuxiliaryStorage};

pub use super::ctx::passes::*;

pub fn collect_strings<'buf>(classes: &[Class<'buf>]) -> StringTable<'buf> {
    StringCollector::new(classes).collect()
}

// haha, you call this too many?
#[allow(clippy::too_many_arguments)]
pub fn lower<'buf, 'aux>(
    storage: &'aux AuxiliaryStorage,
    ty_ctx: TypeCtx<'buf>,
    ty_index: TyIndex<'buf, 'aux, CompleteWasmTy<'buf, 'aux>>,
    method_index: MethodIndex<'buf>,
    method_table: MethodTable,
    vtable: Vtable,
    string_table: StringTable<'buf>,
    field_table: FieldTable<'buf>,
    classes: &[Class<'buf>],
) -> CodegenOutput<'aux> {
    let cg = Codegen::new(
        storage,
        ty_ctx,
        ty_index,
        method_index,
        method_table,
        vtable,
        string_table,
        field_table,
        classes,
    );

    cg.lower()
}
