mod common;

use std::path::Path;

use insta::assert_ron_snapshot;
use paste::paste;
use pretty_assertions::assert_str_eq;

use wacc::ast::dump::{dump_ast, AstDumpFormat};
use wacc::parse::{Cursor, Lexer, Parser};
use wacc::source::{Source, SourceBuffer};

use self::common::Dump;

macro_rules! run_parser_test {
    ($filename:expr) => ({
        let mut source_buf = SourceBuffer::new();
        let mut source = Source::new(&mut source_buf);
        let code = include_bytes!(concat!("parser/", $filename, ".cl"));
        let source_id = source.load_from_string(
            Path::new("tests").join("parser").join($filename.to_string() + ".cl"),
            code.to_vec()
        );
        let source_file = source.get(source_id).unwrap();

        let cursor = Cursor::new(source_file);
        let lexer = Lexer::new(cursor);
        let parser = Parser::new(lexer);

        let parse_result = parser.parse();

        insta::with_settings!({
            description => String::from_utf8_lossy(code),
        }, {
            assert_ron_snapshot!(parse_result);
        });

        let coolc_actual = Dump::from(match parse_result {
            Ok(ast) => {
                let mut buf = vec![];
                dump_ast(
                    &source,
                    AstDumpFormat::Coolc {
                        ignore_types: true,
                    },
                    ast,
                    &mut buf
                )
                .unwrap();

                buf
            },

            Err(_) => b"ERR parsing failed\n".to_vec(),
        });

        let coolc_expected = Dump::from(
            include_bytes!(concat!("parser/", $filename, ".coolc-parse")).as_slice()
        );

        assert_str_eq!(coolc_actual, coolc_expected);
    });

    { $( $filename:ident ),+ $(,)? } => {
        $(
            paste! {
                #[test]
                fn [< test_ $filename >]() {
                    run_parser_test!(concat!(stringify!($filename)));
                }
            }
        )+
    };
}

run_parser_test! {
    fail_empty,
    pass_simple_class,
    pass_arith,
    pass_multi_class,
    pass_fields,
    pass_priority_assign,
    pass_priority_arith,
}
