use std::fmt::Write;

use wasm_bindgen::prelude::*;

use crate::analysis::{self, TypeChecker, TypeckResult};
use crate::codegen::ctx::FieldTable;
use crate::codegen::passes as cg_passes;
use crate::codegen::AuxiliaryStorage;
use crate::errors::{Diagnostic, Diagnostics, Level};
use crate::parse::{Cursor, Lexer, Parser};
use crate::source::{Source, SourceBuffer};
use crate::util::CloneStatic;

fn produce_error_message(errors: impl IntoIterator<Item = String>) -> String {
    let mut result = String::new();
    write!(&mut result, "Compilation failed due to the following errors:").unwrap();

    for message in errors {
        write!(&mut result, "\n{message}").unwrap();
    }

    result
}

fn format_level(level: Level) -> &'static str {
    match level {
        Level::Fatal => "FATAL",
        Level::Error => "ERROR",
        Level::Warn => "WARN ",
        Level::Info => "INFO ",
    }
}

fn format_diagnostic(source: &Source<'_>, diagnostic: Diagnostic) -> String {
    if let Some(span) = &diagnostic.message.span {
        format!("{} {}: {}", format_level(diagnostic.level), span.display(source), diagnostic)
    } else {
        format!("{} {}", format_level(diagnostic.level), diagnostic)
    }
}

fn diagnostics_to_error_message(source: &Source<'_>, diagnostics: Diagnostics) -> String {
    let messages = diagnostics
        .into_vec()
        .into_iter()
        .map(|diagnostic| format_diagnostic(source, diagnostic));

    produce_error_message(messages)
}

#[wasm_bindgen]
pub fn compile_from_string(code: &str) -> Result<Vec<u8>, String> {
    let mut source_buf = SourceBuffer::new();
    let mut source = Source::new(&mut source_buf);
    let source_id = source.load_from_string("main.cl".into(), code.as_bytes().to_vec());
    let source_file = source.get(source_id).unwrap();

    let mut diagnostics = Diagnostics::new();

    let cursor = Cursor::new(source_file);
    let lexer = Lexer::new(cursor);
    let parser = Parser::new(lexer);

    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            diagnostics.error()
                .with_span_and_error(e.clone_static())
                .emit();

            return Err(diagnostics_to_error_message(&source, diagnostics));
        }
    };

    let typeck = TypeChecker::new(&mut diagnostics, ast.classes);
    let TypeckResult {
        classes,
        ctx: ty_ctx,
        sorted: sorted_classes,
    } = typeck.resolve();

    if diagnostics.has_errors() {
        return Err(diagnostics_to_error_message(&source, diagnostics));
    }

    analysis::validate_classes(&source, &ty_ctx, &classes);
    analysis::check_has_main_class(&mut diagnostics, &ty_ctx);

    if diagnostics.has_errors() {
        return Err(diagnostics_to_error_message(&source, diagnostics));
    }

    let ty_index = cg_passes::collect_types(&sorted_classes, &ty_ctx);
    let method_index = cg_passes::enumerate_methods(&sorted_classes, &ty_ctx, &ty_index);
    let method_table = cg_passes::create_method_table(&ty_ctx, &ty_index, &method_index);
    let vtable = cg_passes::create_vtable(&ty_ctx, &ty_index, &method_index, &method_table);
    let mut field_table = FieldTable::new();
    let storage = AuxiliaryStorage::new();
    let ty_index = cg_passes::compute_layout(&storage, &ty_ctx, &ty_index, &mut field_table);
    let string_table = cg_passes::collect_strings(&classes);
    let mut codegen_out = cg_passes::lower(
        &storage,
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
