use std::borrow::Cow;
use std::io::{self, Write};

use wacc::analysis::{self, ClassName, TypeChecker, TypeCtx, TypeckResult};
use wacc::ast::{Class, Program};
use wacc::codegen::ctx::ty::WasmTy;
use wacc::codegen::ctx::{
    CompleteWasmTy, FieldTable, MethodIndex, MethodTable, StringTable, TyIndex, Vtable,
};
use wacc::codegen::{passes as cg_passes, CodegenOutput, AuxiliaryStorage};
use wacc::parse::{Cursor, Lexer, Parser};
use wacc::position::HasSpan;
use wacc::util::CloneStatic;

use super::config::{CodegenOutputFormat, OutputKind, ParserOutputFormat, TypeckOutputFormat};
use super::dump::{dump_ast, dump_tokens, AstDumpFormat};
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
    let OutputKind::Lexer(format) = ctx.config.output else {
        return PassOutput::continue_with_output(lexers);
    };

    for lexer in lexers.drain(..) {
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
    let OutputKind::Parser(format) = ctx.config.output else {
        return PassOutput::continue_with_output(asts);
    };

    let format = match format {
        ParserOutputFormat::Coolc => AstDumpFormat::Coolc { ignore_types: true },
        ParserOutputFormat::Debug => AstDumpFormat::Debug,
        ParserOutputFormat::Ron => AstDumpFormat::Ron,
    };

    for ast in asts.drain(..) {
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
        ..
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
    let OutputKind::Typeck(format) = ctx.config.output else {
        return PassOutput::continue_with_output(classes);
    };

    let format = match format {
        TypeckOutputFormat::Coolc => AstDumpFormat::Coolc { ignore_types: false },
        TypeckOutputFormat::Debug => AstDumpFormat::Debug,
        TypeckOutputFormat::Ron => AstDumpFormat::Ron,
    };

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
) -> PassOutput<TyIndex<'buf, 'buf, WasmTy<'buf>>> {
    PassOutput::continue_with_output(cg_passes::collect_types(sorted, ty_ctx))
}

pub fn enumerate_methods<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    sorted: &[ClassName<'buf>],
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, 'buf, WasmTy<'buf>>,
) -> PassOutput<MethodIndex<'buf>> {
    PassOutput::continue_with_output(cg_passes::enumerate_methods(sorted, ty_ctx, ty_index))
}

pub fn create_method_table<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    ty_ctx: &TypeCtx<'buf>,
    ty_index: &TyIndex<'buf, 'buf, WasmTy<'buf>>,
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
    ty_index: &TyIndex<'buf, 'buf, WasmTy<'buf>>,
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

pub fn compute_layout<'buf, 'aux>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    storage: &'aux AuxiliaryStorage,
    ty_ctx: &TypeCtx<'buf>,
    ty_index: TyIndex<'buf, 'buf, WasmTy<'buf>>,
) -> PassOutput<(FieldTable<'buf>, TyIndex<'buf, 'aux, CompleteWasmTy<'buf, 'aux>>)> {
    let mut field_table = FieldTable::new();
    let ty_index = cg_passes::compute_layout(storage, ty_ctx, &ty_index, &mut field_table);

    PassOutput::continue_with_output((field_table, ty_index))
}

pub fn collect_strings<'buf>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    classes: &[Class<'buf>],
) -> PassOutput<StringTable<'buf>> {
    PassOutput::continue_with_output(cg_passes::collect_strings(classes))
}

// i *was* planning to use a struct here but then decided not to
// that how this function got its 9 parameters
#[allow(clippy::too_many_arguments)]
pub fn codegen<'buf, 'aux>(
    _ctx: &mut RunnerCtx<'buf, '_>,
    storage: &'aux AuxiliaryStorage,
    ty_ctx: TypeCtx<'buf>,
    ty_index: TyIndex<'buf, 'aux, CompleteWasmTy<'buf, 'aux>>,
    method_index: MethodIndex<'buf>,
    method_table: MethodTable,
    vtable: Vtable,
    string_table: StringTable<'buf>,
    field_table: FieldTable<'buf>,
    classes: &[Class<'buf>],
) -> PassOutput<CodegenOutput<'aux>> {
    let mut codegen_out = cg_passes::lower(
        storage,
        ty_ctx,
        ty_index,
        method_index,
        method_table,
        vtable,
        string_table,
        field_table,
        classes,
    );
    codegen_out.module.resolve().unwrap();

    PassOutput::continue_with_output(codegen_out)
}

pub fn output_module_if_asked(
    ctx: &mut RunnerCtx<'_, '_>,
    CodegenOutput { module }: &CodegenOutput,
) -> PassOutput<()> {
    if ctx.config.output != OutputKind::Codegen(CodegenOutputFormat::Debug) {
        return PassOutput::r#continue();
    }

    println!("{:#?}", module);

    PassOutput::stop()
}

pub fn assemble(
    ctx: &mut RunnerCtx<'_, '_>,
    CodegenOutput { mut module }: CodegenOutput,
) -> PassOutput<Vec<u8>> {
    match module.encode() {
        Ok(bytes) => PassOutput::continue_with_output(bytes),

        Err(e) => {
            let message = e.to_string();

            ctx.diagnostics
                .fatal()
                .with_source(Box::new(e))
                .with_message(message)
                .emit();

            PassOutput::stop_with_output(vec![])
        }
    }
}

pub fn write_wasm(ctx: &mut RunnerCtx, wasm: &[u8]) -> PassOutput<()> {
    if ctx.config.output != OutputKind::Codegen(CodegenOutputFormat::Wasm) {
        return PassOutput::r#continue();
    }

    match std::io::stdout().lock().write_all(wasm) {
        Ok(()) => PassOutput::r#continue(),

        Err(e) => {
            let message = e.to_string();

            ctx.diagnostics
                .fatal()
                .with_source(Box::new(e))
                .with_message(message)
                .emit();

            PassOutput::stop()
        }
    }
}
