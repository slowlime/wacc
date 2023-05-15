mod common;

use std::borrow::Cow;
use std::path::Path;

use insta::{assert_debug_snapshot, assert_ron_snapshot};
use paste::paste;
use pretty_assertions::assert_str_eq;
use serde::Serialize;

use wacc::analysis::{self, TypeChecker, TypeckResult};
use wacc::ast;
use wacc::ast::dump::{dump_ast, AstDumpFormat};
use wacc::errors::Diagnostics;
use wacc::parse::{Cursor, Lexer, Parser};
use wacc::position::HasSpan;
use wacc::source::{Source, SourceBuffer};

use self::common::Dump;

fn run_typeck_test_inner(
    code: &'static [u8],
    coolc_expected: &'static [u8],
    filename: &'static str,
    compare_dumps: bool,
) {
    let mut source_buf = SourceBuffer::new();
    let mut source = Source::new(&mut source_buf);
    let source_id = source.load_from_string(
        Path::new("tests")
            .join("typeck")
            .join(filename.to_string() + ".cl"),
        code.to_vec(),
    );
    let source_file = source.get(source_id).unwrap();

    let cursor = Cursor::new(source_file);
    let lexer = Lexer::new(cursor);
    let parser = Parser::new(lexer);
    let ast = parser.parse().unwrap();

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

    let failed = diagnostics.has_errors();
    let diagnostics = diagnostics.into_vec();

    #[derive(Serialize, Debug)]
    struct SnapshotData<'a, 'buf> {
        failed: bool,
        classes: &'a [ast::Class<'buf>],
    }

    insta::with_settings!({
        description => String::from_utf8_lossy(code),
    }, {
        assert_ron_snapshot!(filename.to_owned() + "-ast", SnapshotData {
            failed,
            classes: &classes,
        });

        assert_debug_snapshot!(filename.to_owned() + "-diagnostics", &diagnostics);
    });

    let span = classes
        .iter()
        .map(HasSpan::span)
        .map(Cow::into_owned)
        .reduce(|hull, span| hull.convex_hull(&span))
        .unwrap();
    let program = ast::Program { classes, span };

    if compare_dumps {
        let coolc_actual = Dump::from(match failed {
            false => {
                let mut buf = vec![];
                dump_ast(
                    &source,
                    AstDumpFormat::Coolc {
                        ignore_types: false,
                    },
                    program,
                    &mut buf,
                )
                .unwrap();

                buf
            }

            true => b"ERR typeck failed\n".to_vec(),
        });

        let coolc_expected = Dump::from(coolc_expected);
        assert_str_eq!(coolc_actual, coolc_expected);
    }
}

macro_rules! run_typeck_test {
    ($filename:expr) => ({
        run_typeck_test!($filename, true);
    });

    ($filename:expr, $( $args:tt )+) => ({
        let code = include_bytes!(concat!("typeck/", $filename, ".cl"));
        let coolc_expected = include_bytes!(concat!("typeck/", $filename, ".coolc-typeck"));
        run_typeck_test_inner(code, coolc_expected, $filename, $( $args )+);
    });
}

macro_rules! run_typeck_tests {
    {
        $(
            $filename:ident $(
                ($( $args:tt )+)
            )?
        ),+
        $(,)?
    } => {
        $(
            paste! {
                #[test]
                fn [< test_ $filename >]() {
                    run_typeck_test!(stringify!($filename) $(, $( $args )+ )?);
                }
            }
        )+
    };
}

run_typeck_tests! {
    fail_no_main_class,
    fail_no_main_method,
    fail_main_method_wrong_signature,
    fail_main_method_inherited,
    fail_invalid_override_signature,
    fail_covariant_return_tys,
    fail_field_override,
    fail_inherit_non_existent,
    fail_double_class,
    fail_self_field,
    fail_cyclic_inheritance,
    pass_static_dispatch_self,
    pass_override,
    pass_coexisting_field_method_names,
    pass_new,
    pass_self_derived,
    pass_lexical_scope,

    // wacc does not conform to the reference implementation in this aspect:
    // it does not allow to refer to uninitialized fields, making it a hard error
    fail_field_init_referring_to_uninit(false),
}
