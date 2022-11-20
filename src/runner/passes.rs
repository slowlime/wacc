use std::borrow::Cow;
use std::io;

use crate::analysis::{TypeCtx, TypeChecker, TypeckResult};
use crate::ast::{Class, Program};
use crate::parse::{Cursor, Lexer, Parser};
use crate::position::HasSpan;
use crate::util::CloneStatic;

use super::config::{CompilationStage, OutputFormat};
use super::dump::{dump_ast, dump_tokens};
use super::{PassOutput, RunnerCtx};

pub fn load_files(ctx: &mut RunnerCtx<'_, '_>) -> PassOutput<()> {
    for path in &ctx.config.paths {
        if let Err(e) = ctx.source.borrow_mut().load(path.clone()) {
            ctx.diagnostics
                .error()
                .with_message(format!("could not load file {}", path.display()))
                .with_source(Box::new(e))
                .emit();
        }
    }

    ctx.stop_if_errors(())
}

pub fn scan_files<'buf>(ctx: &mut RunnerCtx<'buf, '_>) -> PassOutput<Vec<Lexer<'buf>>> {
    let lexers = ctx
        .source
        .borrow()
        .iter()
        .map(|src_file| {
            let cursor = Cursor::new(src_file);
            Lexer::new(cursor)
        })
        .collect();

    PassOutput::continue_with_output(lexers)
}

pub fn dump_tokens_if_asked<'buf>(
    ctx: &mut RunnerCtx<'buf, '_>,
    mut lexers: Vec<Lexer<'buf>>,
) -> PassOutput<Vec<Lexer<'buf>>> {
    if ctx.config.output.stage != CompilationStage::Lexer {
        return PassOutput::continue_with_output(lexers);
    }

    for lexer in lexers.drain(..) {
        let format = ctx.config.output.format.unwrap_or(OutputFormat::Coolc);

        if let Err(e) = dump_tokens(format, &ctx.source.borrow(), lexer, io::stdout()) {
            ctx.diagnostics
                .error()
                .with_message("could not dump the tokens to stdout".to_owned())
                .with_source(Box::new(e))
                .emit();
        }
    }

    PassOutput::stop_with_output(lexers)
}

pub fn parse_all<'buf>(
    ctx: &mut RunnerCtx<'buf, '_>,
    mut lexers: Vec<Lexer<'buf>>,
) -> PassOutput<Vec<Program<'buf>>> {
    let mut result = Vec::with_capacity(lexers.len());

    for lexer in lexers.drain(..) {
        let parser = Parser::new(lexer);

        match parser.parse() {
            Ok(program) => result.push(program),

            Err(e) => {
                ctx.diagnostics
                    .error()
                    .with_span_and_error(e.clone_static())
                    .emit();
            }
        }
    }

    ctx.stop_if_errors(result)
}

pub fn dump_asts_if_asked<'buf>(
    ctx: &mut RunnerCtx<'buf, '_>,
    mut asts: Vec<Program<'buf>>,
) -> PassOutput<Vec<Program<'buf>>> {
    if ctx.config.output.stage != CompilationStage::Parser {
        return PassOutput::continue_with_output(asts);
    }

    for ast in asts.drain(..) {
        let format = ctx.config.output.format.unwrap_or(OutputFormat::Coolc);

        if let Err(e) = dump_ast(&ctx.source.borrow(), format, ast, io::stdout()) {
            ctx.diagnostics
                .error()
                .with_message("could not dump an ast to stdout".to_owned())
                .with_source(Box::new(e))
                .emit();
        }
    }

    PassOutput::stop_with_output(asts)
}

pub fn merge_asts<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    asts: Vec<Program<'buf>>,
) -> PassOutput<Vec<Class<'buf>>> {
    PassOutput::continue_with_output(
        asts.into_iter()
            .flat_map(|Program { classes, .. }| classes)
            .collect(),
    )
}

pub fn typeck<'buf>(
    ctx: &mut RunnerCtx<'buf, '_>,
    classes: Vec<Class<'buf>>,
) -> PassOutput<(Vec<Class<'buf>>, TypeCtx<'buf>)> {
    let typeck = TypeChecker::new(&mut ctx.diagnostics, classes);
    let TypeckResult { classes, ctx: ty_ctx } = typeck.resolve();

    ctx.stop_if_errors((classes, ty_ctx))
}

pub fn dump_types_if_asked<'buf>(
    ctx: &mut RunnerCtx<'buf, '_>,
    classes: Vec<Class<'buf>>,
) -> PassOutput<Vec<Class<'buf>>> {
    if ctx.config.output.stage != CompilationStage::Typeck {
        return PassOutput::continue_with_output(classes);
    }

    let format = ctx.config.output.format.unwrap_or(OutputFormat::Coolc);
    let span = classes.iter().map(|class| class.span()).reduce(|lhs, rhs| {
        Cow::Owned(lhs.convex_hull(&rhs))
    }).expect("class array is not empty").into_owned();

    let result = dump_ast(&ctx.source.borrow(), format, Program { classes, span }, io::stdout());

    if let Err(e) = result {
        ctx.diagnostics
            .error()
            .with_message("could not dump the typecked ast to stdout".to_owned())
            .with_source(Box::new(e))
            .emit();
    }

    PassOutput::stop_with_output(vec![])
}
