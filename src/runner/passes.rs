use std::borrow::Cow;
use std::io;

use crate::analysis::{self, ClassName, TypeChecker, TypeCtx, TypeckResult};
use crate::ast::{Class, Program};
use crate::codegen::ctx::ty::WasmTy;
use crate::codegen::ctx::{
    CompleteWasmTy, FieldTable, MethodIndex, MethodTable, StringTable, TyIndex, Vtable,
};
use crate::codegen::{passes as cg_passes, CodegenOutput};
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
) -> PassOutput<(Vec<Class<'buf>>, Vec<ClassName<'buf>>, TypeCtx<'buf>)> {
    let typeck = TypeChecker::new(&mut ctx.diagnostics, classes);
    let TypeckResult {
        classes,
        ctx: ty_ctx,
        sorted,
    } = typeck.resolve();

    ctx.stop_if_errors((classes, sorted, ty_ctx))
}

pub fn validate_classes<'buf>(
    ctx: &mut RunnerCtx<'buf, '_>,
    classes: Vec<Class<'buf>>,
    ty_ctx: TypeCtx<'buf>,
) -> PassOutput<(Vec<Class<'buf>>, TypeCtx<'buf>)> {
    analysis::validate_classes(&ctx.source.borrow(), &ty_ctx, &classes);

    PassOutput::continue_with_output((classes, ty_ctx))
}

pub fn check_has_main_class<'buf>(
    ctx: &mut RunnerCtx<'buf, '_>,
    ty_ctx: TypeCtx<'buf>,
) -> PassOutput<TypeCtx<'buf>> {
    analysis::check_has_main_class(&mut ctx.diagnostics, &ty_ctx);

    ctx.stop_if_errors(ty_ctx)
}

pub fn dump_types_if_asked<'buf>(
    ctx: &mut RunnerCtx<'buf, '_>,
    classes: Vec<Class<'buf>>,
) -> PassOutput<Vec<Class<'buf>>> {
    if ctx.config.output.stage != CompilationStage::Typeck {
        return PassOutput::continue_with_output(classes);
    }

    let format = ctx.config.output.format.unwrap_or(OutputFormat::Coolc);
    let span = classes
        .iter()
        .map(|class| class.span())
        .reduce(|lhs, rhs| Cow::Owned(lhs.convex_hull(&rhs)))
        .map(Cow::into_owned);

    let Some(span) = span else { return PassOutput::stop_with_output(vec![]); };

    let result = dump_ast(
        &ctx.source.borrow(),
        format,
        Program { classes, span },
        io::stdout(),
    );

    if let Err(e) = result {
        ctx.diagnostics
            .error()
            .with_message("could not dump the typecked ast to stdout".to_owned())
            .with_source(Box::new(e))
            .emit();
    }

    PassOutput::stop_with_output(vec![])
}

pub fn collect_types<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    sorted: &[ClassName<'buf>],
    ty_ctx: &TypeCtx<'buf>,
) -> PassOutput<TyIndex<'buf, WasmTy<'buf>>> {
    PassOutput::continue_with_output(cg_passes::collect_types(sorted, ty_ctx))
}

pub fn enumerate_methods<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    sorted: &[ClassName<'buf>],
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
) -> PassOutput<MethodIndex<'buf>> {
    PassOutput::continue_with_output(cg_passes::enumerate_methods(sorted, ty_ctx, ty_index))
}

pub fn create_method_table<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    method_index: &MethodIndex<'buf>,
) -> PassOutput<MethodTable> {
    PassOutput::continue_with_output(cg_passes::create_method_table(
        ty_ctx,
        ty_index,
        method_index,
    ))
}

pub fn create_vtable<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, WasmTy<'buf>>,
    method_index: &MethodIndex<'buf>,
    method_table: &MethodTable,
) -> PassOutput<Vtable> {
    PassOutput::continue_with_output(cg_passes::create_vtable(
        ty_ctx,
        ty_index,
        method_index,
        method_table,
    ))
}

pub fn compute_layout<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    ty_ctx: &TypeCtx<'buf>,
    ty_index: TyIndex<'buf, WasmTy<'buf>>,
) -> PassOutput<(FieldTable<'buf>, TyIndex<'buf, CompleteWasmTy<'buf>>)> {
    let mut field_table = FieldTable::new();
    let ty_index = cg_passes::compute_layout(ty_ctx, &ty_index, &mut field_table);

    PassOutput::continue_with_output((field_table, ty_index))
}

pub fn collect_strings<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    classes: &[Class<'buf>],
) -> PassOutput<StringTable<'buf>> {
    PassOutput::continue_with_output(cg_passes::collect_strings(classes))
}

pub fn codegen<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    ty_ctx: TypeCtx<'buf>,
    ty_index: TyIndex<'buf, CompleteWasmTy<'buf>>,
    method_index: MethodIndex<'buf>,
    method_table: MethodTable,
    vtable: Vtable,
    string_table: StringTable<'buf>,
    field_table: FieldTable<'buf>,
    classes: &[Class<'buf>],
) -> PassOutput<CodegenOutput> {
    PassOutput::continue_with_output(cg_passes::lower(
        ty_ctx,
        ty_index,
        method_index,
        method_table,
        vtable,
        string_table,
        field_table,
        classes,
    ))
}
