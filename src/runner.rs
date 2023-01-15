use std::cell::RefCell;
use std::process::ExitCode;
use std::rc::Rc;

use wacc::errors::Diagnostics;
use wacc::source::{Source, SourceBuffer};

use self::config::{parse_args_or_exit, WaccConfig};
use self::errors::print_diagnostic;

pub mod config;
mod dump;
mod errors;
mod passes;

pub enum CompilationControl {
    Continue,
    Stop,
}

pub struct PassOutput<O> {
    pub output: O,
    pub compilation_control: CompilationControl,
}

impl<O> PassOutput<O> {
    pub fn stop_with_output(output: O) -> Self {
        Self {
            output,
            compilation_control: CompilationControl::Stop,
        }
    }

    pub fn continue_with_output(output: O) -> Self {
        Self {
            output,
            compilation_control: CompilationControl::Continue,
        }
    }
}

impl PassOutput<()> {
    pub fn stop() -> Self {
        Self::stop_with_output(())
    }

    pub fn r#continue() -> Self {
        Self::continue_with_output(())
    }
}

pub struct RunnerCtx<'buf, 'emt> {
    pub config: WaccConfig,
    pub source: Rc<RefCell<Source<'buf>>>,
    pub diagnostics: Diagnostics<'emt>,
}

impl RunnerCtx<'_, '_> {
    pub fn stop_if_errors<O>(&self, output: O) -> PassOutput<O> {
        PassOutput {
            output,
            compilation_control: if self.diagnostics.has_errors() {
                CompilationControl::Stop
            } else {
                CompilationControl::Continue
            },
        }
    }
}

macro_rules! return_if_stopped {
    ($ctx:expr, $e:expr) => {
        match $e {
            PassOutput {
                compilation_control: CompilationControl::Stop,
                ..
            } => {
                return if $ctx.diagnostics.has_errors() {
                    ExitCode::FAILURE
                } else {
                    ExitCode::SUCCESS
                }
            }

            PassOutput { output, .. } => output,
        }
    };
}

fn run(mut ctx: RunnerCtx<'_, '_>) -> ExitCode {
    return_if_stopped!(ctx, passes::load_files(&mut ctx));

    // lexical analysis
    let lexers = return_if_stopped!(ctx, passes::scan_files(&mut ctx));
    let lexers = return_if_stopped!(ctx, passes::dump_tokens_if_asked(&mut ctx, lexers));

    // syntax analysis
    let asts = return_if_stopped!(ctx, passes::parse_all(&mut ctx, lexers));
    let asts = return_if_stopped!(ctx, passes::dump_asts_if_asked(&mut ctx, asts));
    let classes = return_if_stopped!(ctx, passes::merge_asts(&mut ctx, asts));

    // semantic analysis
    let (classes, sorted, ty_ctx) = return_if_stopped!(ctx, passes::typeck(&mut ctx, classes));
    let (classes, ty_ctx) =
        return_if_stopped!(ctx, passes::validate_classes(&mut ctx, classes, ty_ctx));
    let ty_ctx = return_if_stopped!(ctx, passes::check_has_main_class(&mut ctx, ty_ctx));
    let classes = return_if_stopped!(ctx, passes::dump_types_if_asked(&mut ctx, classes));

    // code generation
    let ty_index = return_if_stopped!(ctx, passes::collect_types(&mut ctx, &sorted, &ty_ctx));
    let method_index = return_if_stopped!(
        ctx,
        passes::enumerate_methods(&mut ctx, &sorted, &ty_ctx, &ty_index)
    );
    let method_table = return_if_stopped!(
        ctx,
        passes::create_method_table(&mut ctx, &ty_ctx, &ty_index, &method_index)
    );
    let vtable = return_if_stopped!(
        ctx,
        passes::create_vtable(&mut ctx, &ty_ctx, &ty_index, &method_index, &method_table)
    );
    let (field_table, ty_index) =
        return_if_stopped!(ctx, passes::compute_layout(&mut ctx, &ty_ctx, ty_index));
    let string_table = return_if_stopped!(ctx, passes::collect_strings(&mut ctx, &classes));
    let codegen_out = return_if_stopped!(
        ctx,
        passes::codegen(
            &mut ctx,
            ty_ctx,
            ty_index,
            method_index,
            method_table,
            vtable,
            string_table,
            field_table,
            &classes,
        )
    );
    return_if_stopped!(ctx, passes::output_module_if_asked(&mut ctx, &codegen_out));
    let wasm = return_if_stopped!(ctx, passes::assemble(&mut ctx, codegen_out));
    return_if_stopped!(ctx, passes::write_wasm(&mut ctx, &wasm));

    ExitCode::SUCCESS
}

pub fn prepare_and_run() -> ExitCode {
    let config = parse_args_or_exit();
    let mut source_buf = SourceBuffer::new();
    let source = Rc::new(RefCell::new(Source::new(&mut source_buf)));

    let mut diagnostics = Diagnostics::new();

    diagnostics.set_emitter({
        let source = source.clone();

        Box::new(move |diagnostic| {
            print_diagnostic(&source.borrow(), diagnostic);
        })
    });

    let ctx = RunnerCtx {
        config,
        source,
        diagnostics,
    };

    run(ctx)
}
