use std::fmt::Write;

use wasm_bindgen::prelude::*;

use crate::analysis::{self, TypeChecker, TypeckResult};
use crate::codegen::ctx::FieldTable;
use crate::codegen::passes as cg_passes;
use crate::codegen::AuxiliaryStorage;
use crate::errors::Diagnostics;
use crate::parse::{Cursor, Lexer, Parser};
use crate::source::{Source, SourceBuffer};

fn produce_error_message(errors: impl IntoIterator<Item = String>) -> String {
    let mut result = String::new();
    write!(&mut result, "Compilation failed due to the following errors:").unwrap();

    for message in errors {
        write!(&mut result, "\n{message}").unwrap();
    }

    result
}

fn diagnostics_to_error_message(diagnostics: Diagnostics) -> String {
    let messages = diagnostics
        .into_vec()
        .into_iter()
        .map(|e| e.to_string());

    produce_error_message(messages)
}

#[wasm_bindgen]
pub fn compile_from_string(code: &str) -> Result<Vec<u8>, String> {
    let mut source_buf = SourceBuffer::new();
    let mut source = Source::new(&mut source_buf);
    let source_id = source.load_from_string("main.cl".into(), code.as_bytes().to_vec());
    let source_file = source.get(source_id).unwrap();

    let cursor = Cursor::new(source_file);
    let lexer = Lexer::new(cursor);
    let parser = Parser::new(lexer);
    let ast = parser.parse().map_err(|e| produce_error_message([e.to_string()]))?;
    let mut diagnostics = Diagnostics::new();
    let typeck = TypeChecker::new(&mut diagnostics, ast.classes);
    let TypeckResult {
        classes,
        ctx: ty_ctx,
        sorted: sorted_classes,
    } = typeck.resolve();

    if diagnostics.has_errors() {
        return Err(diagnostics_to_error_message(diagnostics));
    }

    analysis::validate_classes(&source, &ty_ctx, &classes);
    analysis::check_has_main_class(&mut diagnostics, &ty_ctx);

    if diagnostics.has_errors() {
        return Err(diagnostics_to_error_message(diagnostics));
    }

    let ty_index = cg_passes::collect_types(&sorted_classes, &ty_ctx);
    let method_index = cg_passes::enumerate_methods(&sorted_classes, &ty_ctx, &ty_index);
    let method_table = cg_passes::create_method_table(&ty_ctx, &ty_index, &method_index);
    let vtable = cg_passes::create_vtable(&ty_ctx, &ty_index, &method_index, &method_table);
    let mut field_table = FieldTable::new();
    let ty_index = cg_passes::compute_layout(&ty_ctx, &ty_index, &mut field_table);
    let string_table = cg_passes::collect_strings(&classes);
    let mut storage = AuxiliaryStorage::new();
    let mut codegen_out = cg_passes::lower(
        &mut storage,
        ty_ctx,
        ty_index,
        method_index,
        method_table,
        vtable,
        string_table,
        field_table,
        &classes,
    );
    let wasm = codegen_out.module.encode().unwrap();

    Ok(wasm)
}
