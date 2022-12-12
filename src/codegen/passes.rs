use crate::analysis::TypeCtx;
use crate::ast::Class;

use super::ctx::{StringTable, TyIndex, CompleteWasmTy, MethodTable, Vtable, FieldTable, MethodIndex};
use super::string_collector::StringCollector;
use super::{CodegenOutput, Codegen};

pub use super::ctx::passes::*;

pub fn collect_strings<'buf>(classes: &[Class<'buf>]) -> StringTable<'buf> {
    StringCollector::new(classes).collect()
}

pub fn lower<'buf>(
    ty_ctx: TypeCtx<'buf>,
    ty_index: TyIndex<'buf, CompleteWasmTy<'buf>>,
    method_index: MethodIndex<'buf>,
    method_table: MethodTable,
    vtable: Vtable,
    string_table: StringTable<'buf>,
    field_table: FieldTable<'buf>,
    classes: &[Class<'buf>],
) -> CodegenOutput {
    let cg = Codegen::new(
        ty_ctx,
        ty_index,
        method_index,
        method_table,
        vtable,
        string_table,
        field_table,
        classes
    );

    cg.lower()
}
