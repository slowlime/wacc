use std::error::Error;
use std::fmt::{self, Display};
use std::io::{self, Write};

use crate::ast;
use crate::ast::ty::{HasTy, ResolvedTy, Ty};
use crate::parse::token::write_escaped_string;
use crate::position::{PositionPath, Span, Spanned};
use crate::source::Source;
use crate::util::slice_formatter;

#[derive(Debug)]
pub enum AstDumpError {
    Io(io::Error),
    Ron(ron::Error),
}

impl Display for AstDumpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(err) => err.fmt(f),
            Self::Ron(err) => err.fmt(f),
        }
    }
}

impl Error for AstDumpError {}

impl From<io::Error> for AstDumpError {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<ron::Error> for AstDumpError {
    fn from(err: ron::Error) -> Self {
        Self::Ron(err)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AstDumpFormat {
    Coolc {
        ignore_types: bool,
    },
    Debug,
    Ron,
}

pub fn dump_ast<'buf>(
    source: &Source<'buf>,
    format: AstDumpFormat,
    ast: ast::Program<'buf>,
    mut out: impl Write,
) -> Result<(), AstDumpError> {
    match format {
        AstDumpFormat::Coolc { ignore_types } => dump_ast_coolc(source, ast, out, ignore_types)?,
        AstDumpFormat::Debug => writeln!(out, "{:#?}", ast)?,
        AstDumpFormat::Ron => dump_ast_ron(ast, out)?,
    }

    Ok(())
}

fn dump_ast_ron<'buf>(ast: ast::Program<'buf>, out: impl Write) -> Result<(), ron::Error> {
    use ron::{extensions::Extensions, ser::PrettyConfig};

    ron::Options::default().to_writer_pretty(
        out,
        &ast,
        PrettyConfig::new().extensions(Extensions::IMPLICIT_SOME | Extensions::UNWRAP_NEWTYPES),
    )
}

fn dump_ast_coolc<'buf>(
    source: &Source<'buf>,
    ast: ast::Program<'buf>,
    out: impl Write,
    ignore_types: bool,
) -> io::Result<()> {
    use ast::Visitor;

    struct Dumper<'src, W: Write> {
        source: &'src Source<'src>,
        out: W,
        level: usize,
        ignore_types: bool,
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

        fn write_ty(&mut self, ty: Option<&Ty<'buf>>) -> io::Result<()> {
            self.indented_line(|this| match ty {
                _ if this.ignore_types => write!(&mut this.out, ": _no_type"),
                None | Some(Ty::Unresolved(_)) => write!(&mut this.out, ": _no_type"),

                Some(Ty::Resolved(ty)) => match ty {
                    ResolvedTy::Builtin(builtin) => write!(&mut this.out, ": {}", builtin),
                    ResolvedTy::SelfType { .. } => write!(&mut this.out, ": SELF_TYPE"),
                    ResolvedTy::Class(name) => write!(&mut this.out, ": {}", slice_formatter(name)),
                    ResolvedTy::Function(ty) => write!(&mut this.out, ": {}", ty),
                    ResolvedTy::Bottom => write!(&mut this.out, ": {}", ty),
                    ResolvedTy::Untyped => write!(&mut this.out, ": {}", ty),
                },
            })
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
                this.visit_ty_name(&class.name)?;

                match &class.inherits {
                    Some(ty_name) => this.visit_ty_name(ty_name)?,
                    None => this.indented_line(|this| write!(&mut this.out, "Object"))?,
                };

                this.indented_line(|this| {
                    write_escaped_string(
                        format!("{}", PositionPath::new(class.span.start.src, this.source))
                            .as_bytes(),
                        &mut this.out,
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

                this.visit_ty_name(&method.return_ty)?;
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

            self.write_ty(expr.ty().as_deref())?;

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
                Receiver::SelfType { .. } | Receiver::Dynamic { .. } => {
                    write!(&mut this.out, "_dispatch")
                }

                Receiver::Static { .. } => write!(&mut this.out, "_static_dispatch"),
            })?;

            self.with_nested(|this| {
                this.visit_receiver(&expr.receiver)?;
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
            self.with_nested(|this| this.visit_ty_name(&expr.ty_name))
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
            self.write_span(&expr.name.0.span)?;
            self.indented_line(|this| write!(&mut this.out, "_object"))?;
            self.with_nested(|this| this.visit_name(&expr.name))
        }

        fn visit_formal(&mut self, formal: &ast::Formal<'buf>) -> Self::Output {
            self.write_span(&formal.span)?;
            self.indented_line(|this| write!(&mut this.out, "_formal"))?;
            self.with_nested(|this| {
                this.visit_name(&formal.name)?;
                this.visit_ty_name(&formal.ty_name)?;

                Ok(())
            })
        }

        fn visit_receiver(&mut self, recv: &ast::Receiver<'buf>) -> Self::Output {
            use ast::Receiver;

            match recv {
                Receiver::SelfType {
                    method_name_span,
                    ty,
                } => {
                    // synthesize a "self" NameExpr
                    let mock_self = ast::Expr::Name(ast::NameExpr {
                        name: ast::Name(Spanned {
                            value: b"self".as_slice().into(),
                            span: method_name_span.clone(),
                        }),
                        binding_id: None,
                        ty: Some(ty.clone()),
                    });
                    self.visit_expr(&mock_self)?;
                }

                Receiver::Dynamic(object) => self.visit_expr(object)?,

                Receiver::Static {
                    object, ty_name, ..
                } => {
                    self.visit_expr(object)?;
                    self.visit_ty_name(ty_name)?;
                }
            }

            Ok(())
        }

        fn visit_case_arm(&mut self, arm: &ast::CaseArm<'buf>) -> Self::Output {
            self.write_span(&arm.span)?;
            self.indented_line(|this| write!(&mut this.out, "_branch"))?;
            self.with_nested(|this| {
                this.visit_name(&arm.name)?;
                this.visit_ty_name(&arm.binding_ty_name)?;
                this.visit_expr(&arm.expr)?;

                Ok(())
            })
        }

        fn visit_ty_name(&mut self, ty_name: &ast::TyName<'buf>) -> Self::Output {
            self.visit_name(&ty_name.0)
        }

        fn visit_name(&mut self, name: &ast::Name<'buf>) -> Self::Output {
            self.indented_line(|this| this.out.write_all(&name.0.value))
        }

        fn visit_binding(&mut self, binding: &ast::Binding<'buf>) -> Self::Output {
            self.visit_name(&binding.name)?;
            self.visit_ty_name(&binding.ty_name)?;

            match &binding.init {
                Some(expr) => self.visit_expr(expr)?,
                None => self.write_empty_expr()?,
            }

            Ok(())
        }

        fn visit_int_lit(&mut self, expr: &ast::IntLit) -> Self::Output {
            self.write_span(&expr.0.span)?;
            self.indented_line(|this| write!(&mut this.out, "_int"))?;
            self.with_nested(|this| {
                this.indented_line(|this| write!(&mut this.out, "{}", expr.0.value))
            })
        }

        fn visit_string_lit(&mut self, expr: &ast::StringLit<'buf>) -> Self::Output {
            self.write_span(&expr.0.span)?;
            self.indented_line(|this| write!(&mut this.out, "_string"))?;
            self.with_nested(|this| {
                this.indented_line(|this| write_escaped_string(&expr.0.value, &mut this.out))
            })
        }

        fn visit_bool_lit(&mut self, expr: &ast::BoolLit) -> Self::Output {
            self.write_span(&expr.0.span)?;
            self.indented_line(|this| write!(&mut this.out, "_bool"))?;
            self.with_nested(|this| {
                this.indented_line(|this| {
                    write!(
                        &mut this.out,
                        "{}",
                        match expr.0.value {
                            true => 1,
                            false => 0,
                        }
                    )
                })
            })
        }
    }

    let mut dumper = Dumper {
        source,
        out,
        level: 0,
        ignore_types,
    };

    dumper.visit_program(&ast)
}
