use std::borrow::Cow;
use std::collections::HashSet;
use std::error::Error;
use std::fmt::{self, Display};

use itertools::{Itertools, PeekNth};
use tracing::{instrument, trace};

use crate::ast::ty::UnresolvedTy;
use crate::ast::{self, BinOpKind, Expr, Name, UnOpKind, TyName};
use crate::parse::lexer::{Lexer, LexerError};
use crate::position::{HasSpan, Span, Spanned};
use crate::parse::token::{Symbol, Token, TokenType};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParserError<'buf> {
    UnexpectedToken {
        expected: Cow<'static, [TokenType]>,
        actual: Token<'buf>,
    },

    UppercasedName(TyName<'buf>),
    LowercasedTyName(Name<'buf>),

    LexerError(LexerError),
}

impl From<LexerError> for ParserError<'_> {
    fn from(e: LexerError) -> Self {
        Self::LexerError(e)
    }
}

impl Display for ParserError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { expected, actual } => {
                write!(f, "Encountered an unexpected token: {}", actual.ty())?;

                match expected.len() {
                    0 => Ok(()),
                    1 => write!(f, " (expected {})", &expected[0]),
                    2 => write!(f, " (expected {} or {})", &expected[0], &expected[1]),

                    _ => {
                        write!(f, " (expected ")?;

                        for ty in expected.iter().take(expected.len() - 1) {
                            write!(f, "{}, ", ty)?;
                        }

                        write!(f, ", or {})", &expected[expected.len() - 1])
                    }
                }
            }

            Self::UppercasedName(name) => {
                write!(f, "An object name cannot start with an uppercase letter: `{}`", name)
            }

            Self::LowercasedTyName(name) => {
                write!(f, "A type name cannot start with a lowercase letter: `{}`", name)
            }

            Self::LexerError(err) => write!(f, "{}", err),
        }
    }
}

impl Error for ParserError<'_> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::LexerError(err) => Some(err),
            _ => None,
        }
    }
}

impl HasSpan for ParserError<'_> {
    fn span(&self) -> Cow<'_, Span> {
        match self {
            Self::UnexpectedToken { actual, .. } => actual.span(),
            Self::UppercasedName(ty_name) => ty_name.span(),
            Self::LowercasedTyName(name) => name.span(),
            Self::LexerError(e) => e.span(),
        }
    }
}

trait Matcher {
    fn matches<'t>(&self, token: &Token<'t>) -> bool;

    fn expected_tokens(&self) -> Cow<'static, [TokenType]>;
}

impl<const N: usize> Matcher for &'static [TokenType; N] {
    fn matches<'t>(&self, token: &Token<'t>) -> bool {
        self.contains(&token.ty())
    }

    fn expected_tokens(&self) -> Cow<'static, [TokenType]> {
        self.as_slice().into()
    }
}

impl<'buf, const N: usize> Matcher for &'buf [Symbol; N] {
    fn matches<'t>(&self, token: &Token<'t>) -> bool {
        match token.ty() {
            TokenType::Symbol(sym) => self.contains(&sym),
            _ => false,
        }
    }

    fn expected_tokens(&self) -> Cow<'static, [TokenType]> {
        self.iter().copied().map(TokenType::Symbol).collect()
    }
}

impl Matcher for TokenType {
    fn matches<'t>(&self, token: &Token<'t>) -> bool {
        self == &token.ty()
    }

    fn expected_tokens(&self) -> Cow<'static, [TokenType]> {
        vec![*self].into()
    }
}

impl Matcher for Symbol {
    fn matches<'t>(&self, token: &Token<'t>) -> bool {
        TokenType::Symbol(*self) == token.ty()
    }

    fn expected_tokens(&self) -> Cow<'static, [TokenType]> {
        vec![TokenType::Symbol(*self)].into()
    }
}

macro_rules! select {
    ($self:ident : { $( $matcher:expr => $arm:expr, )+ _ => $default:expr }) => ({
        match $self.lexer.peek() {
            $( Some(Ok(token)) if $matcher.matches(token) => $arm, )+
            _ => $default,
        }
    });

    ($self:ident : { $( $matcher:expr => $arm:expr, )+ _ => @error, }) => ({
        select!($self: { $( $matcher => $arm, )+ _ => {
            let expected: HashSet<_> = [$( $matcher.expected_tokens().iter().copied(), )+]
                .into_iter()
                .flatten()
                .collect();

            return Err(ParserError::UnexpectedToken {
                expected: expected.into_iter().collect::<Vec<_>>().into(),
                actual: $self.lexer.peek().unwrap().clone().unwrap(),
            });
        }})
    });
}

fn format_expected(tokens: Cow<'static, [TokenType]>) -> String {
    tokens.iter().map(|token| format!("{}", token)).join(", ")
}

pub struct Parser<'buf> {
    lexer: PeekNth<Lexer<'buf>>,
}

impl<'buf> Parser<'buf> {
    pub fn new(lexer: Lexer<'buf>) -> Self {
        Self {
            lexer: itertools::peek_nth(lexer),
        }
    }

    fn expect_generic<M, F, G, R>(
        &mut self,
        matcher: M,
        on_match: F,
        on_fail: G,
    ) -> Result<R, ParserError<'buf>>
    where
        M: Matcher,
        F: FnOnce(Token<'buf>) -> Result<R, ParserError<'buf>>,
        G: FnOnce(&'_ Token<'buf>, M) -> Result<R, ParserError<'buf>>,
        R: fmt::Debug,
    {
        trace!(token = ?self.lexer.peek());

        let result = match self.lexer.peek() {
            Some(Ok(token)) if matcher.matches(token) => {
                on_match(self.lexer.next().unwrap().unwrap())
            }
            Some(Ok(token)) => on_fail(token, matcher),
            Some(Err(_)) => Err(self.lexer.next().unwrap().err().unwrap().into()),
            None => panic!("peeking after retrieving the Eof token"),
        };

        trace!(result = ?result);

        result
    }

    #[instrument(
        level = "trace",
        ret,
        skip(self, matcher),
        fields(matcher = format_expected(matcher.expected_tokens()))
    )]
    fn expect(&mut self, matcher: impl Matcher) -> Result<Token<'buf>, ParserError<'buf>> {
        self.expect_generic(matcher, Ok, |token, matcher| {
            Err(ParserError::UnexpectedToken {
                expected: matcher.expected_tokens(),
                actual: token.clone(),
            })
        })
    }

    #[instrument(
        level = "trace",
        ret,
        skip(self, matcher),
        fields(matcher = format_expected(matcher.expected_tokens()))
    )]
    fn try_consume(
        &mut self,
        matcher: impl Matcher,
    ) -> Result<Option<Token<'buf>>, ParserError<'buf>> {
        self.expect_generic(matcher, |token| Ok(Some(token)), |_, _| Ok(None))
    }

    #[instrument(
        level = "trace",
        ret,
        skip(self, matcher),
        fields(matcher = format_expected(matcher.expected_tokens()))
    )]
    fn matches_nth(&mut self, n: usize, matcher: impl Matcher) -> bool {
        trace!(token = ?self.lexer.peek_nth(n));

        matches!(self.lexer.peek_nth(n), Some(Ok(token)) if matcher.matches(token))
    }

    #[instrument(level = "trace", skip(self), ret)]
    pub fn parse(mut self) -> Result<ast::Program<'buf>, ParserError<'buf>> {
        let mut classes = Vec::new();

        loop {
            classes.push(self.parse_class()?);
            let semicolon = self.expect(Symbol::Semicolon)?;

            if self.try_consume(TokenType::Eof)?.is_some() {
                let span = classes[0].span.convex_hull(&semicolon.span);

                return Ok(ast::Program { classes, span });
            }
        }
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_class(&mut self) -> Result<ast::Class<'buf>, ParserError<'buf>> {
        let class = self.expect(Symbol::Class)?;
        let name = self.parse_ty_name()?;

        let inherits = match self.try_consume(Symbol::Inherits)? {
            Some(_) => Some(self.parse_ty_name()?),
            _ => None,
        };

        self.expect(Symbol::BraceLeft)?;
        let mut features = Vec::new();

        let brace_right = loop {
            if let Some(brace_right) = self.try_consume(Symbol::BraceRight)? {
                break brace_right;
            }

            features.push(self.parse_feature()?);
            self.expect(Symbol::Semicolon)?;
        };

        let span = class.span.convex_hull(&brace_right.span);
        let ty = UnresolvedTy::Named(name.clone()).into();

        Ok(ast::Class {
            name,
            inherits,
            features,
            span,
            ty,
        })
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_name(&mut self) -> Result<ast::Name<'buf>, ParserError<'buf>> {
        let ident = self.expect(TokenType::Ident)?;
        let spanned: Spanned<Cow<'buf, [u8]>> = ident.try_into().unwrap();

        if spanned.value[0].is_ascii_uppercase() {
            Err(ParserError::UppercasedName(TyName(Name(spanned))))
        } else {
            Ok(Name(spanned))
        }
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_ty_name(&mut self) -> Result<ast::TyName<'buf>, ParserError<'buf>> {
        let ident = self.expect(TokenType::Ident)?;
        let spanned: Spanned<Cow<'buf, [u8]>> = ident.try_into().unwrap();

        if spanned.value[0].is_ascii_lowercase() {
            Err(ParserError::LowercasedTyName(Name(spanned)))
        } else {
            Ok(TyName(Name(spanned)))
        }
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_feature(&mut self) -> Result<ast::Feature<'buf>, ParserError<'buf>> {
        let name = self.parse_name()?;

        Ok(select!(self: {
            Symbol::ParenLeft => ast::Feature::Method(self.parse_method(name)?),
            Symbol::Colon => ast::Feature::Field(ast::Field(self.parse_binding(name)?)),
            _ => @error,
        }))
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_method(&mut self, name: Name<'buf>) -> Result<ast::Method<'buf>, ParserError<'buf>> {
        self.expect(Symbol::ParenLeft)?;
        let mut params = Vec::new();
        let mut comma_required = false;

        loop {
            if self.try_consume(Symbol::ParenRight)?.is_some() {
                break;
            }

            if comma_required {
                self.expect(Symbol::Comma)?;
            } else {
                comma_required = true;
            }

            params.push(self.parse_formal()?);
        }

        self.expect(Symbol::Colon)?;

        let return_ty = self.parse_ty_name()?;

        self.expect(Symbol::BraceLeft)?;
        let body = self.parse_expr()?;
        let brace_right = self.expect(Symbol::BraceRight)?;
        let span = name.0.span.convex_hull(&brace_right.span);

        let ty = {
            let args = params.iter().map(|ast::Formal { ty, .. }| ty.clone()).collect();
            let ret = Box::new(UnresolvedTy::Named(return_ty.clone()).into());

            UnresolvedTy::Function { args, ret }.into()
        };

        Ok(ast::Method {
            name,
            params,
            return_ty,
            body,
            span,
            ty,
        })
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_formal(&mut self) -> Result<ast::Formal<'buf>, ParserError<'buf>> {
        let name = self.parse_name()?;
        self.expect(Symbol::Colon)?;
        let ty_name = self.parse_ty_name()?;
        let span = name.0.span.convex_hull(&ty_name.span());
        let ty = UnresolvedTy::Named(ty_name.clone()).into();

        Ok(ast::Formal { name, ty_name, span, ty })
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_binding(&mut self, name: Name<'buf>) -> Result<ast::Binding<'buf>, ParserError<'buf>> {
        self.expect(Symbol::Colon)?;
        let ty_name = self.parse_ty_name()?;
        let ty = UnresolvedTy::Named(ty_name.clone()).into();

        let (span, init) = if self.try_consume(Symbol::ArrowLeft)?.is_some() {
            let expr = self.parse_expr()?;
            let span = name.0.span.convex_hull(&expr.span());

            (span, Some(expr))
        } else {
            let span = name.0.span.convex_hull(&ty_name.span());

            (span, None)
        };

        Ok(ast::Binding {
            name,
            ty_name,
            init,
            span,
            ty,
        })
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        self.parse_expr_assign()
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_assign(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        if self.matches_nth(0, TokenType::Ident) && self.matches_nth(1, Symbol::ArrowLeft) {
            let name = self.parse_name()?;
            self.expect(Symbol::ArrowLeft)?;
            let expr = self.parse_expr_assign()?;
            let span = name.0.span.convex_hull(&expr.span());

            Ok(Box::new(Expr::Assignment(ast::Assignment {
                name,
                expr,
                span,
            })))
        } else {
            self.parse_expr_not()
        }
    }

    fn parse_un_op<R, D>(
        &mut self,
        matcher: impl Matcher,
        mut recurse: R,
        mut descend: D,
    ) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>>
    where
        R: FnMut(&mut Self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>>,
        D: FnMut(&mut Self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>>,
    {
        if let Some(sym) = self.try_consume(matcher)? {
            let op = match sym.ty() {
                TokenType::Symbol(sym) => UnOpKind::try_from(sym).unwrap(),
                _ => unreachable!(),
            };

            let expr = recurse(self)?;
            let span = sym.span.convex_hull(&expr.span());
            let ty = None;

            Ok(Box::new(Expr::UnOp(ast::UnOpExpr { op, expr, span, ty })))
        } else {
            descend(self)
        }
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_not(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        self.parse_un_op(Symbol::Not, Self::parse_expr_not, Self::parse_expr_cmp)
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_cmp(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        const CMP_OPS: [Symbol; 3] = [Symbol::LessEquals, Symbol::Less, Symbol::Equals];

        let lhs = self.parse_expr_addsub()?;

        if let Some(sym) = self.try_consume(&CMP_OPS)? {
            let op = match sym.ty() {
                TokenType::Symbol(sym) => BinOpKind::try_from(sym).unwrap(),
                _ => unreachable!(),
            };

            let rhs = self.parse_expr_addsub()?;
            let span = lhs.span().convex_hull(&rhs.span());
            let ty = None;

            Ok(Box::new(Expr::BinOp(ast::BinOpExpr { op, lhs, rhs, span, ty })))
        } else {
            Ok(lhs)
        }
    }

    fn parse_bin_op_lassoc<D>(
        &mut self,
        matcher: impl Matcher + Copy,
        mut descend: D,
    ) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>>
    where
        D: FnMut(&mut Self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>>,
    {
        let mut lhs = descend(self)?;

        while let Some(sym) = self.try_consume(matcher)? {
            let op = match sym.ty() {
                TokenType::Symbol(sym) => BinOpKind::try_from(sym).unwrap(),
                _ => unreachable!(),
            };

            let rhs = descend(self)?;
            let span = lhs.span().convex_hull(&rhs.span());
            let ty = None;

            lhs = Box::new(Expr::BinOp(ast::BinOpExpr { op, lhs, rhs, span, ty }));
        }

        Ok(lhs)
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_addsub(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        self.parse_bin_op_lassoc(&[Symbol::Plus, Symbol::Minus], Self::parse_expr_muldiv)
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_muldiv(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        self.parse_bin_op_lassoc(&[Symbol::Asterisk, Symbol::Slash], Self::parse_expr_is_void)
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_is_void(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        self.parse_un_op(
            Symbol::IsVoid,
            Self::parse_expr_is_void,
            Self::parse_expr_compl,
        )
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_compl(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        self.parse_un_op(
            Symbol::Tilde,
            Self::parse_expr_compl,
            Self::parse_expr_static_dispatch,
        )
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_static_dispatch(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let mut object = self.parse_expr_dynamic_dispatch()?;

        while self.try_consume(Symbol::At)?.is_some() {
            let ty_name = self.parse_ty_name()?;
            self.expect(Symbol::Dot)?;
            object = self.parse_method_call(
                Some(object.span().into_owned()),
                ast::Receiver::Static { object, ty_name },
            )?;
        }

        Ok(object)
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_method_call(
        &mut self,
        receiver_span: Option<Span>,
        receiver: ast::Receiver<'buf>,
    ) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let method = self.parse_name()?;
        self.expect(Symbol::ParenLeft)?;
        let mut args = Vec::new();

        let paren = loop {
            if let Some(paren) = self.try_consume(Symbol::ParenRight)? {
                break paren;
            }

            if !args.is_empty() {
                self.expect(Symbol::Comma)?;
            }

            args.push(self.parse_expr()?);
        };

        let span = receiver_span
            .as_ref()
            .unwrap_or(&method.0.span)
            .convex_hull(&paren.span);

        Ok(Box::new(Expr::Call(ast::Call {
            receiver,
            method,
            args,
            span,
            ty: None,
        })))
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_dynamic_dispatch(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let mut object = self.parse_expr_atom()?;

        while self.try_consume(Symbol::Dot)?.is_some() {
            object = self.parse_method_call(
                Some(object.span().into_owned()),
                ast::Receiver::Dynamic(object),
            )?;
        }

        Ok(object)
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_atom(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        select!(self: {
            Symbol::If => self.parse_if(),
            Symbol::While => self.parse_while(),
            Symbol::BraceLeft => self.parse_block(),
            Symbol::Let => self.parse_let(),
            Symbol::Case => self.parse_case(),
            Symbol::New => self.parse_new(),
            Symbol::ParenLeft => self.parse_paren(),

            TokenType::Ident => self.parse_expr_self_call(),
            TokenType::Int => self.parse_int_lit(),
            TokenType::String => self.parse_string_lit(),
            &[Symbol::True, Symbol::False] => self.parse_bool_lit(),

            _ => @error,
        })
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_if(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let r#if = self.expect(Symbol::If)?;
        let antecedent = self.parse_expr()?;
        self.expect(Symbol::Then)?;
        let consequent = self.parse_expr()?;
        self.expect(Symbol::Else)?;
        let alternative = self.parse_expr()?;
        let fi = self.expect(Symbol::Fi)?;

        let span = r#if.span.convex_hull(&fi.span);

        Ok(Box::new(Expr::If(ast::If {
            antecedent,
            consequent,
            alternative,
            span,
            ty: None,
        })))
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_while(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let r#while = self.expect(Symbol::While)?;
        let condition = self.parse_expr()?;
        self.expect(Symbol::Loop)?;
        let body = self.parse_expr()?;
        let pool = self.expect(Symbol::Pool)?;

        let span = r#while.span.convex_hull(&pool.span);

        Ok(Box::new(Expr::While(ast::While {
            condition,
            body,
            span,
        })))
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_block(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let brace_left = self.expect(Symbol::BraceLeft)?;
        let mut body = Vec::new();

        let brace_right = loop {
            body.push(self.parse_expr()?);
            self.expect(Symbol::Semicolon)?;

            if let Some(brace_right) = self.try_consume(Symbol::BraceRight)? {
                break brace_right;
            }
        };

        let span = brace_left.span.convex_hull(&brace_right.span);

        Ok(Box::new(Expr::Block(ast::Block { body, span })))
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_let(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let r#let = self.expect(Symbol::Let)?;
        let mut bindings = Vec::new();

        loop {
            let name = self.parse_name()?;
            bindings.push(self.parse_binding(name)?);

            if self.try_consume(Symbol::In)?.is_some() {
                break;
            } else {
                self.expect(Symbol::Comma)?;
            }
        }

        let expr = self.parse_expr()?;

        Ok(bindings.into_iter().enumerate().rfold(expr, |expr, (i, binding)| {
            let start = if i == 0 { &r#let.span } else { &binding.span };
            let span = start.convex_hull(&expr.span());

            Box::new(Expr::Let(ast::Let {
                binding,
                expr,
                span,
            }))
        }))
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_case(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let case = self.expect(Symbol::Case)?;
        let scrutinee = self.parse_expr()?;
        self.expect(Symbol::Of)?;
        let mut arms = Vec::new();

        let esac = loop {
            let name = self.parse_name()?;
            self.expect(Symbol::Colon)?;
            let binding_ty_name = self.parse_ty_name()?;
            self.expect(Symbol::Implies)?;
            let expr = self.parse_expr()?;
            let semicolon = self.expect(Symbol::Semicolon)?;

            let span = name.0.span.convex_hull(&semicolon.span);
            let binding_ty = UnresolvedTy::Named(binding_ty_name.clone()).into();

            arms.push(ast::CaseArm {
                name,
                binding_ty_name,
                expr,
                span,
                binding_ty,
            });

            if let Some(esac) = self.try_consume(Symbol::Esac)? {
                break esac;
            }
        };

        let span = case.span.convex_hull(&esac.span);

        Ok(Box::new(Expr::Case(ast::Case {
            scrutinee,
            arms,
            span,
            ty: None,
        })))
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_new(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let new = self.expect(Symbol::New)?;
        let ty_name = self.parse_ty_name()?;
        let span = new.span.convex_hull(&ty_name.span());
        let ty = UnresolvedTy::Named(ty_name.clone()).into();

        Ok(Box::new(Expr::New(ast::New { ty_name, span, ty })))
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_paren(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        self.expect(Symbol::ParenLeft)?;
        let expr = self.parse_expr()?;
        self.expect(Symbol::ParenRight)?;

        Ok(expr)
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_expr_self_call(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        if self.matches_nth(1, Symbol::ParenLeft) {
            self.parse_method_call(None, ast::Receiver::SelfType)
        } else {
            let name = self.parse_name()?;
            let ty = None;

            Ok(Box::new(Expr::Name(ast::NameExpr { name, ty })))
        }
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_int_lit(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let token = self.expect(TokenType::Int)?.try_into().unwrap();

        Ok(Box::new(Expr::Int(ast::IntLit(token))))
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_string_lit(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let token = self.expect(TokenType::String)?.try_into().unwrap();

        Ok(Box::new(Expr::String(ast::StringLit(token))))
    }

    #[instrument(level = "trace", skip(self), ret)]
    fn parse_bool_lit(&mut self) -> Result<Box<ast::Expr<'buf>>, ParserError<'buf>> {
        let token = self
            .expect(&[Symbol::True, Symbol::False])?
            .try_into()
            .unwrap();

        Ok(Box::new(Expr::Bool(ast::BoolLit(token))))
    }
}
