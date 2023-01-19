mod common;

use std::fs::File;
use std::io::Write;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
use std::path::Path;
use std::process::{Command, Stdio};
use std::thread::scope;
use std::time::Duration;

use command_fds::{CommandFdExt, FdMapping};
use nix::fcntl::{FcntlArg, FdFlag};
use paste::paste;
use pretty_assertions::assert_str_eq;

use process_control::{ChildExt, Control};
use wacc::analysis::{self, TypeChecker, TypeckResult};
use wacc::codegen::ctx::FieldTable;
use wacc::codegen::passes as cg_passes;
use wacc::errors::Diagnostics;
use wacc::parse::{Cursor, Lexer, Parser};
use wacc::source::{Source, SourceBuffer};

use self::common::Dump;

fn run_wasm(wasm: &[u8], input: &[u8]) -> Dump<'static> {
    static RUN_TEST_JS: &[u8] = include_bytes!("codegen/run-test.js");
    static TEST_RUN_LIMIT: Duration = Duration::from_secs(5 * 60);

    let (wasm_pipe_rd, wasm_pipe_wr) = nix::unistd::pipe().unwrap();

    // Safety: we've just made ourselves a brand new pipe,
    // so both fds are definitely valid here, and `close` is enough in terms of bookkeeping
    let wasm_pipe_wr = unsafe { OwnedFd::from_raw_fd(wasm_pipe_wr) };
    let wasm_pipe_rd = unsafe { OwnedFd::from_raw_fd(wasm_pipe_rd) };

    let (input_pipe_rd, input_pipe_wr) = nix::unistd::pipe().unwrap();

    // Safety: same as above
    let input_pipe_wr = unsafe { OwnedFd::from_raw_fd(input_pipe_wr) };
    let input_pipe_rd = unsafe { OwnedFd::from_raw_fd(input_pipe_rd) };

    // make sure the child doesn't haunt pipe_wr
    nix::fcntl::fcntl(
        wasm_pipe_wr.as_raw_fd(),
        FcntlArg::F_SETFD(FdFlag::FD_CLOEXEC),
    )
    .unwrap();
    nix::fcntl::fcntl(
        input_pipe_wr.as_raw_fd(),
        FcntlArg::F_SETFD(FdFlag::FD_CLOEXEC),
    )
    .unwrap();

    let mut child = Command::new("node")
        .arg("--experimental-wasm-gc")
        .arg("--input-type=module")
        .arg("-")
        .stdin(Stdio::piped())
        .stderr(Stdio::inherit())
        .stdout(Stdio::piped())
        .fd_mappings(vec![
            FdMapping {
                parent_fd: wasm_pipe_rd.as_raw_fd(),
                child_fd: 3,
            },
            FdMapping {
                parent_fd: input_pipe_rd.as_raw_fd(),
                child_fd: 4,
            },
        ])
        .unwrap()
        .spawn()
        .unwrap();

    // gotta close what we don't need
    drop(wasm_pipe_rd);
    drop(input_pipe_rd);

    child.stdin.take().unwrap().write_all(RUN_TEST_JS).unwrap();

    let out = scope(|s| {
        s.spawn(|| {
            File::from(wasm_pipe_wr).write_all(wasm).unwrap();
            File::from(input_pipe_wr).write_all(input).unwrap();
        });

        match child
            .controlled_with_output()
            .time_limit(TEST_RUN_LIMIT)
            .strict_errors()
            .terminate_for_timeout()
            .wait()
            .unwrap()
        {
            Some(out) => out,
            None => panic!("The child process timed out"),
        }
    });

    assert!(out.status.success());

    out.stdout.into()
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
    let ty_index = cg_passes::compute_layout(&ty_ctx, &ty_index, &mut field_table);
    let string_table = cg_passes::collect_strings(&classes);
    let mut codegen_out = cg_passes::lower(
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

    let actual = run_wasm(&wasm, &[]);
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
}
