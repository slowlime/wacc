use std::io::{self, Write};
use std::path::{Path, PathBuf};

use clap::{Args, Parser as ClapParser, Subcommand, ValueEnum};
use color_eyre::eyre::{eyre, Context, Report};

use crate::ast;
use crate::cursor::Cursor;
use crate::lexer::{Lexer, LexerError};
use crate::parser::{Parser, ParserError};
use crate::position::{PositionPath, Span, Spanned, HasSpan};
use crate::source::{Source, SourceBuffer};
use crate::token::{self, write_escaped_string, Symbol, SymbolCategory, Token, TokenValue};

#[derive(ClapParser, Debug, Clone)]
#[command(version)]
pub struct WaccCli {
    #[command(subcommand)]
    pub command: WaccCommand,
}

#[derive(Subcommand, Debug, Clone)]
pub enum WaccCommand {
    /// Dump the debug representation of input at a particular compilation stage
    Dump(DumpArgs),
}

#[derive(Args, Debug, Clone)]
pub struct DumpArgs {
    /// The input files
    pub paths: Vec<PathBuf>,

    /// The compiler stage to stop at
    #[arg(short, long, value_enum)]
    pub stage: DumpStage,

    /// The output format to use
    #[arg(short, long, value_enum, default_value_t = DumpFormat::Coolc)]
    pub format: DumpFormat,
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DumpStage {
    Lexer,
    Parser,
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DumpFormat {
    Coolc,
    Debug,
}

impl WaccCli {
    pub fn run(self) -> Result<(), Report> {
        match self.command {
            WaccCommand::Dump(dump_args) => DumpRunner::new(dump_args).run(),
        }
    }
}

struct DumpRunner {
    paths: Vec<PathBuf>,
    stage: DumpStage,
    format: DumpFormat,
}

impl DumpRunner {
    pub fn new(
        DumpArgs {
            paths,
            stage,
            format,
        }: DumpArgs,
    ) -> Self {
        Self {
            paths,
            stage,
            format,
        }
    }

    pub fn run(self) -> Result<(), Report> {
        let paths = &self.paths;
        let mut buf = SourceBuffer::new();
        let mut source = Source::new(&mut buf);

        for path in paths {
            let id = source
                .load(path.clone())
                .wrap_err_with(|| eyre!("Could not load the file {}", path.display()))?;
            let src_file = source.get(id).unwrap();
            let cursor = Cursor::new(src_file);
            let lexer = Lexer::new(cursor);

            if self.stage <= DumpStage::Lexer {
                if self.stage == DumpStage::Lexer {
                    Self::dump_tokens(self.format, path, lexer, io::stdout().lock())
                        .wrap_err("Could not dump tokens to stdout")?;
                }

                continue;
            }

            let parser = Parser::new(lexer);
            let ast = parser.parse();

            if self.stage <= DumpStage::Parser {
                if self.stage == DumpStage::Parser {
                    Self::dump_ast(&source, self.format, ast, io::stdout().lock())
                        .wrap_err("Could not dump the ast to stdut")?;
                }

                continue;
            }
        }

        Ok(())
    }

    fn dump_tokens<'src, I>(
        format: DumpFormat,
        path: &Path,
        tokens: I,
        mut out: impl Write,
    ) -> io::Result<()>
    where
        I: Iterator<Item = Result<Token<'src>, LexerError>>,
    {
        match format {
            DumpFormat::Coolc => Self::dump_tokens_coolc(path, tokens, out),
            DumpFormat::Debug => write!(out, "{:#?}", tokens.collect::<Vec<_>>()),
        }
    }

    fn dump_tokens_coolc<'src, I>(path: &Path, tokens: I, mut out: impl Write) -> io::Result<()>
    where
        I: Iterator<Item = Result<Token<'src>, LexerError>>,
    {
        let name = format!("{}", path.display());
        write!(out, "#name ")?;
        write_escaped_string(name.as_bytes(), &mut out)?;
        writeln!(out)?;

        for token in tokens {
            let token = match token {
                Ok(Token {
                    value: TokenValue::Eof,
                    ..
                }) => continue,

                Ok(token) => token,

                Err(e) => {
                    let span = e.span();
                    write!(out, "#{} ERROR ", span.start.line)?;
                    let msg = format!("{}", e);
                    write_escaped_string(msg.as_bytes(), &mut out)?;
                    writeln!(out)?;

                    continue;
                }
            };

            write!(out, "#{} ", token.span.start.line)?;

            match token.value {
                TokenValue::Eof => unreachable!(),

                TokenValue::Int(i) => write!(out, "INT_CONST {}", i)?,

                TokenValue::Ident(id) => {
                    if id.first().map(u8::is_ascii_uppercase).unwrap_or(false) {
                        out.write_all(b"TYPEID ")?;
                    } else {
                        out.write_all(b"OBJECTID ")?;
                    }

                    out.write_all(id)?;
                }

                TokenValue::Symbol(sym) => match sym.category() {
                    // booleans enjoy special treatment by the reference lexer
                    _ if [Symbol::True, Symbol::False].contains(&sym) => write!(
                        out,
                        "BOOL_CONST {}",
                        if sym == Symbol::True { "true" } else { "false" }
                    )?,

                    SymbolCategory::Keyword => {
                        write!(out, "{}", sym.as_str().to_ascii_uppercase())?
                    }

                    // weird edge-cases (since they're multibyte, I guess?)
                    _ if sym == Symbol::ArrowLeft => write!(out, "ASSIGN")?,
                    _ if sym == Symbol::Implies => write!(out, "DARROW")?,
                    _ if sym == Symbol::LessEquals => write!(out, "LE")?,

                    _ => write!(out, "'{}'", sym.as_str())?,
                },

                TokenValue::String(s) => {
                    write!(out, "STR_CONST ")?;
                    token::write_escaped_string(&s, &mut out)?;
                }
            }

            writeln!(out)?;
        }

        Ok(())
    }

    fn dump_ast<'src>(
        source: &Source<'src>,
        format: DumpFormat,
        ast: Result<ast::Program<'src>, ParserError<'src>>,
        mut out: impl Write,
    ) -> io::Result<()> {
        match format {
            DumpFormat::Coolc => Self::dump_ast_coolc(source, ast, out),
            DumpFormat::Debug => write!(out, "{:#?}", ast),
        }
    }

    fn dump_ast_coolc<'src>(
        source: &Source<'src>,
        ast: Result<ast::Program<'src>, ParserError<'src>>,
        mut out: impl Write,
    ) -> io::Result<()> {
        use ast::Visitor;

        struct Dumper<'src, W: Write> {
            // a hack: visit_receiver needs to know the line when dumping Receiver::SelfType
            call_span: Option<Span>,
            source: &'src Source<'src>,
            out: W,
            level: usize,
        }

        impl<'buf, W: Write> Dumper<'buf, W> {
            fn write_indent(&mut self) -> io::Result<()> {
                write!(&mut self.out, "{:width$}", "", width = 2 * self.level)
            }

            fn indented_line<F>(&mut self, f: F) -> io::Result<()>
            where
                F: FnOnce(&mut Self) -> io::Result<()>,
            {
                self.write_indent()?;
                f(&mut *self)?;
                writeln!(&mut self.out)?;

                Ok(())
            }

            fn write_span(&mut self, span: &Span) -> io::Result<()> {
                self.indented_line(|this| write!(&mut this.out, "#{}", span.start.line))
            }

            fn with_nested<F>(&mut self, f: F) -> io::Result<()>
            where
                F: FnOnce(&mut Self) -> io::Result<()>,
            {
                self.level += 1;
                f(&mut *self)?;
                self.level -= 1;

                Ok(())
            }

            fn write_empty_expr(&mut self) -> io::Result<()> {
                self.indented_line(|this| write!(&mut this.out, "#0"))?;
                self.indented_line(|this| write!(&mut this.out, "_no_expr"))?;
                self.indented_line(|this| write!(&mut this.out, ": _no_type"))?;

                Ok(())
            }
        }

        impl<'buf, W: Write> Visitor<'buf> for Dumper<'buf, W> {
            type Output = io::Result<()>;

            fn visit_program(&mut self, program: &ast::Program<'buf>) -> Self::Output {
                self.write_span(&program.span)?;
                self.indented_line(|this| write!(&mut this.out, "_program"))?;
                self.with_nested(|this| {
                    for class in &program.classes {
                        this.visit_class(class)?;
                    }

                    Ok(())
                })
            }

            fn visit_class(&mut self, class: &ast::Class<'buf>) -> Self::Output {
                self.write_span(&class.span)?;
                self.indented_line(|this| write!(&mut this.out, "_class"))?;
                self.with_nested(|this| {
                    this.visit_name(&class.name)?;

                    match &class.inherits {
                        Some(name) => this.visit_name(name)?,
                        None => this.indented_line(|this| write!(&mut this.out, "Object"))?,
                    };

                    this.indented_line(|this| {
                        write!(
                            &mut this.out,
                            "{}",
                            PositionPath::new(class.span.start.src, this.source)
                        )
                    })?;

                    this.indented_line(|this| write!(&mut this.out, "("))?;

                    for feature in &class.features {
                        this.visit_feature(feature)?;
                    }

                    this.indented_line(|this| write!(&mut this.out, ")"))?;

                    Ok(())
                })
            }

            fn visit_feature(&mut self, feature: &ast::Feature<'buf>) -> Self::Output {
                match feature {
                    ast::Feature::Method(method) => self.visit_method(method),
                    ast::Feature::Field(field) => self.visit_field(field),
                }
            }

            fn visit_method(&mut self, method: &ast::Method<'buf>) -> Self::Output {
                self.write_span(&method.span)?;
                self.indented_line(|this| write!(&mut this.out, "_method"))?;
                self.with_nested(|this| {
                    this.visit_name(&method.name)?;

                    for formal in &method.params {
                        this.visit_formal(formal)?;
                    }

                    this.visit_name(&method.return_ty)?;
                    this.visit_expr(&method.body)?;

                    Ok(())
                })
            }

            fn visit_field(&mut self, field: &ast::Field<'buf>) -> Self::Output {
                self.write_span(&field.0.span)?;
                self.indented_line(|this| write!(&mut this.out, "_attr"))?;
                self.with_nested(|this| {
                    this.visit_binding(&field.0)?;

                    Ok(())
                })
            }

            fn visit_expr(&mut self, expr: &ast::Expr<'buf>) -> Self::Output {
                use ast::Expr;

                match expr {
                    Expr::Assignment(expr) => self.visit_assignment(expr)?,
                    Expr::Call(expr) => self.visit_call(expr)?,
                    Expr::If(expr) => self.visit_if(expr)?,
                    Expr::While(expr) => self.visit_while(expr)?,
                    Expr::Block(expr) => self.visit_block(expr)?,
                    Expr::Let(expr) => self.visit_let(expr)?,
                    Expr::Case(expr) => self.visit_case(expr)?,
                    Expr::New(expr) => self.visit_new(expr)?,
                    Expr::BinOp(expr) => self.visit_bin_op(expr)?,
                    Expr::UnOp(expr) => self.visit_un_op(expr)?,
                    Expr::Name(expr) => self.visit_name_expr(expr)?,
                    Expr::Int(expr) => self.visit_int_lit(expr)?,
                    Expr::String(expr) => self.visit_string_lit(expr)?,
                    Expr::Bool(expr) => self.visit_bool_lit(expr)?,
                };

                self.indented_line(|this| write!(&mut this.out, ": _no_type"))?;

                Ok(())
            }

            fn visit_assignment(&mut self, expr: &ast::Assignment<'buf>) -> Self::Output {
                self.write_span(&expr.span)?;
                self.indented_line(|this| write!(&mut this.out, "_assign"))?;
                self.with_nested(|this| {
                    this.visit_name(&expr.name)?;
                    this.visit_expr(&expr.expr)?;

                    Ok(())
                })
            }

            fn visit_call(&mut self, expr: &ast::Call<'buf>) -> Self::Output {
                use ast::Receiver;

                self.write_span(&expr.span)?;

                self.indented_line(|this| match &expr.receiver {
                    Receiver::SelfType | Receiver::Dynamic { .. } => {
                        write!(&mut this.out, "_dispatch")
                    }

                    Receiver::Static { .. } => write!(&mut this.out, "_static_dispatch"),
                })?;

                self.with_nested(|this| {
                    this.call_span = Some(expr.span.clone());
                    this.visit_receiver(&expr.receiver)?;
                    this.call_span = None;
                    this.visit_name(&expr.method)?;
                    this.indented_line(|this| write!(&mut this.out, "("))?;

                    for arg in &expr.args {
                        this.visit_expr(arg)?;
                    }

                    this.indented_line(|this| write!(&mut this.out, ")"))?;

                    Ok(())
                })
            }

            fn visit_if(&mut self, expr: &ast::If<'buf>) -> Self::Output {
                self.write_span(&expr.span)?;
                self.indented_line(|this| write!(&mut this.out, "_cond"))?;
                self.with_nested(|this| {
                    this.visit_expr(&expr.antecedent)?;
                    this.visit_expr(&expr.consequent)?;
                    this.visit_expr(&expr.alternative)?;

                    Ok(())
                })
            }

            fn visit_while(&mut self, expr: &ast::While<'buf>) -> Self::Output {
                self.write_span(&expr.span)?;
                self.indented_line(|this| write!(&mut this.out, "_loop"))?;
                self.with_nested(|this| {
                    this.visit_expr(&expr.condition)?;
                    this.visit_expr(&expr.body)?;

                    Ok(())
                })
            }

            fn visit_block(&mut self, expr: &ast::Block<'buf>) -> Self::Output {
                self.write_span(&expr.span)?;
                self.indented_line(|this| write!(&mut this.out, "_block"))?;
                self.with_nested(|this| {
                    for expr in &expr.body {
                        this.visit_expr(expr)?;
                    }

                    Ok(())
                })
            }

            fn visit_let(&mut self, expr: &ast::Let<'buf>) -> Self::Output {
                self.write_span(&expr.span)?;
                self.indented_line(|this| write!(&mut this.out, "_let"))?;
                self.with_nested(|this| {
                    this.visit_binding(&expr.binding)?;
                    this.visit_expr(&expr.expr)?;

                    Ok(())
                })
            }

            fn visit_case(&mut self, expr: &ast::Case<'buf>) -> Self::Output {
                self.write_span(&expr.span)?;
                self.indented_line(|this| write!(&mut this.out, "_typcase"))?;
                self.with_nested(|this| {
                    this.visit_expr(&expr.scrutinee)?;

                    for arm in &expr.arms {
                        this.visit_case_arm(arm)?;
                    }

                    Ok(())
                })
            }

            fn visit_new(&mut self, expr: &ast::New<'buf>) -> Self::Output {
                self.write_span(&expr.span)?;
                self.indented_line(|this| write!(&mut this.out, "_new"))?;
                self.with_nested(|this| this.visit_name(&expr.ty))
            }

            fn visit_bin_op(&mut self, expr: &ast::BinOpExpr<'buf>) -> Self::Output {
                use ast::BinOpKind;

                self.write_span(&expr.span)?;
                self.indented_line(|this| {
                    write!(
                        &mut this.out,
                        "{}",
                        match &expr.op {
                            // the consistency here is impressive
                            BinOpKind::Add => "_plus",
                            BinOpKind::Subtract => "_sub",
                            BinOpKind::Multiply => "_mul",
                            BinOpKind::Divide => "_divide",
                            BinOpKind::LessThan => "_lt",
                            BinOpKind::LessEquals => "_leq",
                            BinOpKind::Equals => "_eq",
                        }
                    )
                })?;

                self.with_nested(|this| {
                    this.visit_expr(&expr.lhs)?;
                    this.visit_expr(&expr.rhs)?;

                    Ok(())
                })
            }

            fn visit_un_op(&mut self, expr: &ast::UnOpExpr<'buf>) -> Self::Output {
                use ast::UnOpKind;

                self.write_span(&expr.span)?;
                self.indented_line(|this| {
                    write!(
                        &mut this.out,
                        "{}",
                        match &expr.op {
                            UnOpKind::IsVoid => "_isvoid",
                            UnOpKind::Complement => "_neg",
                            UnOpKind::Not => "_comp",
                        }
                    )
                })?;

                self.with_nested(|this| this.visit_expr(&expr.expr))
            }

            fn visit_name_expr(&mut self, expr: &ast::NameExpr<'buf>) -> Self::Output {
                self.write_span(&expr.0 .0.span)?;
                self.indented_line(|this| write!(&mut this.out, "_object"))?;
                self.with_nested(|this| this.visit_name(&expr.0))
            }

            fn visit_formal(&mut self, formal: &ast::Formal<'buf>) -> Self::Output {
                self.write_span(&formal.span)?;
                self.indented_line(|this| write!(&mut this.out, "_formal"))?;
                self.with_nested(|this| {
                    this.visit_name(&formal.name)?;
                    this.visit_name(&formal.ty)?;

                    Ok(())
                })
            }

            fn visit_receiver(&mut self, recv: &ast::Receiver<'buf>) -> Self::Output {
                use ast::Receiver;

                match recv {
                    Receiver::SelfType => {
                        // synthesize a "self" NameExpr
                        let mock_self = ast::Expr::Name(ast::NameExpr(ast::Name(Spanned {
                            value: b"name",
                            span: self.call_span.take().unwrap(),
                        })));
                        self.visit_expr(&mock_self)?;
                    }

                    Receiver::Dynamic(object) => self.visit_expr(object)?,

                    Receiver::Static { object, ty } => {
                        self.visit_expr(object)?;
                        self.visit_name(ty)?;
                    }
                }

                Ok(())
            }

            fn visit_case_arm(&mut self, arm: &ast::CaseArm<'buf>) -> Self::Output {
                self.write_span(&arm.span)?;
                self.indented_line(|this| write!(&mut this.out, "_branch"))?;
                self.with_nested(|this| {
                    this.visit_name(&arm.name)?;
                    this.visit_name(&arm.ty)?;
                    this.visit_expr(&arm.expr)?;

                    Ok(())
                })
            }

            fn visit_name(&mut self, name: &ast::Name<'buf>) -> Self::Output {
                self.indented_line(|this| this.out.write_all(name.0.value))
            }

            fn visit_binding(&mut self, binding: &ast::Binding<'buf>) -> Self::Output {
                self.visit_name(&binding.name)?;
                self.visit_name(&binding.ty)?;

                match &binding.init {
                    Some(expr) => self.visit_expr(expr)?,
                    None => self.write_empty_expr()?,
                }

                Ok(())
            }

            fn visit_int_lit(&mut self, expr: &ast::IntLit) -> Self::Output {
                self.write_span(&expr.0.span)?;
                self.indented_line(|this| write!(&mut this.out, "_int"))?;
                self.with_nested(|this| write!(&mut this.out, "{}", expr.0.value))
            }

            fn visit_string_lit(&mut self, expr: &ast::StringLit<'buf>) -> Self::Output {
                self.write_span(&expr.0.span)?;
                self.indented_line(|this| write!(&mut this.out, "_string"))?;
                self.with_nested(|this| write_escaped_string(&expr.0.value, &mut this.out))
            }

            fn visit_bool_lit(&mut self, expr: &ast::BoolLit) -> Self::Output {
                self.write_span(&expr.0.span)?;
                self.indented_line(|this| write!(&mut this.out, "_bool"))?;
                self.with_nested(|this| write!(&mut this.out, "{}", match expr.0.value {
                    true => 1,
                    false => 0,
                }))
            }
        }

        match ast {
            Ok(ref ast) => {
                let mut dumper = Dumper {
                    call_span: None,
                    source,
                    out,
                    level: 0,
                };

                dumper.visit_program(ast)
            }

            Err(e) => {
                let span = e.span();
                write!(out, "#{} ERROR ", span.start.line)?;
                let msg = format!("{}", e);
                write_escaped_string(msg.as_bytes(), &mut out)?;
                writeln!(out)?;

                Ok(())
            }
        }
    }
}
