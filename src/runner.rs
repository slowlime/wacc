use std::cell::RefCell;
use std::process::ExitCode;
use std::rc::Rc;

use crate::errors::Diagnostics;
use crate::source::{Source, SourceBuffer};

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
    let lexers = return_if_stopped!(ctx, passes::scan_files(&mut ctx));
    let lexers = return_if_stopped!(ctx, passes::dump_tokens_if_asked(&mut ctx, lexers));
    let asts = return_if_stopped!(ctx, passes::parse_all(&mut ctx, lexers));
    let asts = return_if_stopped!(ctx, passes::dump_asts_if_asked(&mut ctx, asts));
    let classes = return_if_stopped!(ctx, passes::merge_asts(&mut ctx, asts));
    let (classes, ty_ctx) = return_if_stopped!(ctx, passes::typeck(&mut ctx, classes));
    let classes = return_if_stopped!(ctx, passes::check_all_types_resolved(&mut ctx, classes));
    let classes = return_if_stopped!(ctx, passes::dump_types_if_asked(&mut ctx, classes));

    let (_, _) = (classes, ty_ctx);

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

    return run(ctx);
}
