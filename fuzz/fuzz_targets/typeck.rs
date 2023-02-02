#![no_main]

use libfuzzer_sys::fuzz_target;

use wacc::analysis::{self, TypeChecker, TypeckResult};
use wacc::errors::Diagnostics;
use wacc::parse::{Cursor, Lexer, Parser};
use wacc::source::{Source, SourceBuffer};

fuzz_target!(|code: &[u8]| {
    let mut source_buf = SourceBuffer::new();
    let mut source = Source::new(&mut source_buf);
    let source_id = source.load_from_string("fuzz-input.cl".into(), code.to_vec());
    let source_file = source.get(source_id).unwrap();

    let cursor = Cursor::new(source_file);
    let lexer = Lexer::new(cursor);
    let parser = Parser::new(lexer);
    let Ok(ast) = parser.parse() else { return };

    let mut diagnostics = Diagnostics::new();

    let typeck = TypeChecker::new(&mut diagnostics, ast.classes);
    let TypeckResult {
        classes,
        ctx: ty_ctx,
        ..
    } = typeck.resolve();

    if !diagnostics.has_errors() {
        analysis::validate_classes(&source, &ty_ctx, &classes);
        analysis::check_has_main_class(&mut diagnostics, &ty_ctx);
    }
});
