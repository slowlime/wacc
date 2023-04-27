mod common;

use std::panic::{self, AssertUnwindSafe};
use std::path::Path;

use once_cell::sync::Lazy;
use paste::paste;
use pretty_assertions::assert_str_eq;

use wacc::analysis::{self, TypeChecker, TypeckResult};
use wacc::codegen::ctx::FieldTable;
use wacc::codegen::{passes as cg_passes, AuxiliaryStorage};
use wacc::errors::Diagnostics;
use wacc::parse::{Cursor, Lexer, Parser};
use wacc::source::{Source, SourceBuffer};

use self::common::Dump;

fn run_wasm(wasm: Vec<u8>, input: &'static [u8]) -> Dump<'static> {
    const OUTPUT_LINES_KEY: &str = "__outputLines";

    static RUN_TEST_JS: &str = include_str!("codegen/run-test.js");
    static V8_PLATFORM: Lazy<v8::SharedRef<v8::Platform>> = Lazy::new(|| {
        v8::V8::set_flags_from_string("--experimental-wasm-gc");

        let platform = v8::new_default_platform(0, false).make_shared();
        v8::V8::initialize_platform(platform.clone());
        v8::V8::initialize();
        v8::V8::assert_initialized();

        platform
    });

    Lazy::force(&V8_PLATFORM);

    let isolate = &mut v8::Isolate::new(Default::default());

    let scope = &mut v8::HandleScope::new(isolate);
    let ctx = v8::Context::new(scope);
    let scope = &mut v8::ContextScope::new(scope, ctx);

    let wasm_backing_store = v8::ArrayBuffer::new_backing_store_from_vec(wasm).make_shared();
    let wasm_array_buf = v8::ArrayBuffer::with_backing_store(scope, &wasm_backing_store);
    let wasm_array =
        v8::Uint8Array::new(scope, wasm_array_buf, 0, wasm_backing_store.len()).unwrap();
    let wasm_key = v8::String::new(scope, "wasm").unwrap();

    scope
        .get_current_context()
        .global(scope)
        .set(scope, wasm_key.into(), wasm_array.into())
        .unwrap();

    let input_lines: Vec<_> = input
        .split(|&c| c == b'\n')
        .map(|line| {
            let line_backing_store =
                v8::ArrayBuffer::new_backing_store_from_vec(line.into()).make_shared();
            let line_array_buf = v8::ArrayBuffer::with_backing_store(scope, &wasm_backing_store);

            v8::Uint8Array::new(scope, line_array_buf, 0, line_backing_store.len())
                .unwrap()
                .into()
        })
        .collect();
    let input_lines = v8::Array::new_with_elements(scope, input_lines.as_slice());
    let input_key = v8::String::new(scope, "inputLines").unwrap();

    scope
        .get_current_context()
        .global(scope)
        .set(scope, input_key.into(), input_lines.into())
        .unwrap();

    fn js_write_to_stdout<'a>(
        scope: &mut v8::HandleScope<'a>,
        args: v8::FunctionCallbackArguments<'a>,
        mut ret: v8::ReturnValue,
    ) {
        let res = panic::catch_unwind({
            let mut scope = AssertUnwindSafe(&mut *scope);

            move || {
                let scope = &mut **scope;
                let output_lines_key = v8::String::new(scope, OUTPUT_LINES_KEY).unwrap();
                let output_lines = scope
                    .get_current_context()
                    .global(scope)
                    .get(scope, output_lines_key.into())
                    .unwrap();
                let output_lines = v8::Local::<v8::Array>::try_from(output_lines).unwrap();

                let line = args.get(0);
                assert!(line.is_uint8_array());
                output_lines
                    .set_index(scope, output_lines.length(), line)
                    .unwrap();
            }
        });

        ret.set_undefined();

        if res.is_err() {
            // the panic hooks have already been executed at this point,
            // one of which prints the panic message
            scope.terminate_execution();
        }
    }

    fn js_encode_utf8<'a>(
        scope: &mut v8::HandleScope<'a>,
        args: v8::FunctionCallbackArguments<'a>,
        mut ret: v8::ReturnValue,
    ) {
        let res = panic::catch_unwind({
            let mut scope = AssertUnwindSafe(&mut *scope);

            move || {
                let scope = &mut **scope;

                let string = match v8::Local::<v8::String>::try_from(args.get(0)) {
                    Ok(string) => string,
                    Err(e) => {
                        let msg = v8::String::new(scope, &e.to_string()).unwrap();
                        let exc = v8::Exception::type_error(scope, msg);
                        scope.throw_exception(exc);

                        return;
                    }
                };

                let string: Vec<_> = string.to_rust_string_lossy(scope).bytes().collect();
                let backing_store =
                    v8::ArrayBuffer::new_backing_store_from_vec(string).make_shared();
                let array_buf = v8::ArrayBuffer::with_backing_store(scope, &backing_store);
                let array =
                    v8::Uint8Array::new(scope, array_buf, 0, array_buf.byte_length()).unwrap();

                ret.set(array.into());
            }
        });

        if res.is_err() {
            scope.terminate_execution();
        }
    }

    fn js_decode_utf8<'a>(
        scope: &mut v8::HandleScope<'a>,
        args: v8::FunctionCallbackArguments<'a>,
        mut ret: v8::ReturnValue,
    ) {
        let res = panic::catch_unwind({
            let mut scope = AssertUnwindSafe(&mut *scope);

            move || {
                let scope = &mut **scope;

                let array = match v8::Local::<v8::Uint8Array>::try_from(args.get(0)) {
                    Ok(array) => array,
                    Err(e) => {
                        let msg = v8::String::new(scope, &e.to_string()).unwrap();
                        let exc = v8::Exception::type_error(scope, msg);
                        scope.throw_exception(exc);

                        return;
                    }
                };

                let array: Vec<_> = array
                    .buffer(scope)
                    .unwrap()
                    .get_backing_store()
                    .iter()
                    .map(|cell| cell.get())
                    .collect();
                let string = String::from_utf8_lossy(&array);
                let string = v8::String::new(scope, &string).unwrap();

                ret.set(string.into());
            }
        });

        if res.is_err() {
            scope.terminate_execution();
        }
    }

    let output_lines_key = v8::String::new(scope, OUTPUT_LINES_KEY).unwrap();
    let output_lines_value = v8::Array::new(scope, 0);

    scope
        .get_current_context()
        .global(scope)
        .set(scope, output_lines_key.into(), output_lines_value.into())
        .unwrap();

    let write_to_stdout_cb = v8::FunctionBuilder::<v8::Function>::new(js_write_to_stdout)
        .build(scope)
        .unwrap();
    let write_to_stdout_key = v8::String::new(scope, "writeToStdout").unwrap();

    scope
        .get_current_context()
        .global(scope)
        .set(scope, write_to_stdout_key.into(), write_to_stdout_cb.into())
        .unwrap();

    let encode_utf8_cb = v8::FunctionBuilder::<v8::Function>::new(js_encode_utf8)
        .build(scope)
        .unwrap();
    let encode_utf8_key = v8::String::new(scope, "encodeUtf8").unwrap();

    scope
        .get_current_context()
        .global(scope)
        .set(scope, encode_utf8_key.into(), encode_utf8_cb.into())
        .unwrap();

    let decode_utf8_cb = v8::FunctionBuilder::<v8::Function>::new(js_decode_utf8)
        .build(scope)
        .unwrap();
    let decode_utf8_key = v8::String::new(scope, "decodeUtf8").unwrap();

    scope
        .get_current_context()
        .global(scope)
        .set(scope, decode_utf8_key.into(), decode_utf8_cb.into())
        .unwrap();

    let code = v8::String::new(scope, RUN_TEST_JS).unwrap();
    let script = v8::Script::compile(scope, code, None).unwrap();

    let scope = &mut v8::TryCatch::new(scope);
    let result = script.run(scope)
        .and_then(|value| v8::Local::<v8::Promise>::try_from(value).ok());

    let exc = 'exc: {
        if let Some(promise) = result {
            while promise.state() == v8::PromiseState::Pending {
                v8::Platform::pump_message_loop(&V8_PLATFORM, scope, true);
                scope.perform_microtask_checkpoint();
            }

            if promise.state() == v8::PromiseState::Rejected {
                break 'exc Some(promise.result(scope));
            }
        };

        scope.exception()
    };

    if let Some(exc) = exc {
        let msg = exc
            .to_detail_string(scope)
            .unwrap()
            .to_rust_string_lossy(scope);
        panic!("The script has thrown an uncaught exception: {}", msg);
    }

    let output_lines = scope
        .get_current_context()
        .global(scope)
        .get(scope, output_lines_key.into())
        .unwrap();
    let output_lines = v8::Local::<v8::Array>::try_from(output_lines).unwrap();
    let output_lines: Vec<_> = (0..output_lines.length())
        .map(|i| output_lines.get_index(scope, i).unwrap())
        .map(|local| v8::Local::<v8::Uint8Array>::try_from(local).unwrap())
        .collect();
    let output_size: usize = output_lines.iter().map(|array| array.byte_length()).sum();
    let mut output = vec![0; output_size];
    let mut written_size = 0;

    for output_line in output_lines {
        written_size += output_line.copy_contents(&mut output[written_size..]);
    }

    output.into()
}

fn run_codegen_test_inner(code: &'static [u8], expected: &'static [u8], filename: &'static str) {
    let mut source_buf = SourceBuffer::new();
    let mut source = Source::new(&mut source_buf);
    let source_id = source.load_from_string(
        Path::new("tests")
            .join("codegen")
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
        sorted: sorted_classes,
    } = typeck.resolve();
    assert!(
        !diagnostics.has_errors(),
        "typeck failed!\n{:?}",
        diagnostics.into_vec()
    );

    analysis::validate_classes(&source, &ty_ctx, &classes);
    analysis::check_has_main_class(&mut diagnostics, &ty_ctx);
    assert!(
        !diagnostics.has_errors(),
        "semantic analysis failed!\n{:?}",
        diagnostics.into_vec()
    );

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

    let actual = run_wasm(wasm, &[]);
    let expected = Dump::from(expected);

    assert_str_eq!(actual, expected);
}

macro_rules! run_codegen_test {
    ($filename:expr) => {{
        let code = include_bytes!(concat!("codegen/", $filename, ".cl"));
        let expected = include_bytes!(concat!("codegen/", $filename, ".stdout"));
        run_codegen_test_inner(code, expected, $filename);
    }};
}

macro_rules! run_codegen_tests {
    { $( $filename:ident ),* $(,)? } => {
        $(
            paste! {
                #[test]
                fn [< test_ $filename >]() {
                    run_codegen_test!(stringify!($filename));
                }
            }
        )*
    };
}

run_codegen_tests! {
    print_string,
    print_int,
    inheritance,
    default_init,
    string_ops,
    case,
    isvoid,
    arith,
    eq,
    lexical_scope,
}
