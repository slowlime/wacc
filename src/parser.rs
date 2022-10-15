use std::borrow::Cow;
use std::collections::HashSet;

use itertools::PeekNth;

use crate::ast::{self, BinOpKind, Expr, Name, UnOpKind};
use crate::lexer::{Lexer, LexerError};
use crate::token::{Symbol, Token, TokenType};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParserError<'a> {
    UnexpectedToken {
        expected: Cow<'static, [TokenType]>,
        actual: Token<'a>,
    },

    LexerError(LexerError),
}

impl From<LexerError> for ParserError<'_> {
    fn from(e: LexerError) -> Self {
        Self::LexerError(e)
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

impl<'a, const N: usize> Matcher for &'a [Symbol; N] {
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

pub struct Parser<'a> {
    lexer: PeekNth<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: itertools::peek_nth(lexer),
        }
    }

    fn expect_generic<M, F, G, R>(
        &mut self,
        matcher: M,
        on_match: F,
        on_fail: G,
    ) -> Result<R, ParserError<'a>>
    where
        M: Matcher,
        F: FnOnce(Token<'a>) -> Result<R, ParserError<'a>>,
        G: FnOnce(&'_ Token<'a>, M) -> Result<R, ParserError<'a>>,
    {
        match self.lexer.peek() {
            Some(Ok(token)) if matcher.matches(token) => {
                on_match(self.lexer.next().unwrap().unwrap())
            }
            Some(Ok(token)) => on_fail(token, matcher),
            Some(Err(_)) => Err(self.lexer.next().unwrap().err().unwrap().into()),
            None => panic!("peeking after retrieving the Eof token"),
        }
    }

    fn expect(&mut self, matcher: impl Matcher) -> Result<Token<'a>, ParserError<'a>> {
        self.expect_generic(matcher, Ok, |token, matcher| {
            Err(ParserError::UnexpectedToken {
                expected: matcher.expected_tokens(),
                actual: token.clone(),
            })
        })
    }

    fn try_consume(&mut self, matcher: impl Matcher) -> Result<Option<Token<'a>>, ParserError<'a>> {
        self.expect_generic(matcher, |token| Ok(Some(token)), |_, _| Ok(None))
    }

    fn matches_nth(&mut self, n: usize, matcher: impl Matcher) -> bool {
        matches!(self.lexer.peek_nth(n), Some(Ok(token)) if matcher.matches(token))
    }

    pub fn parse(&mut self) -> Result<ast::Program<'a>, ParserError<'a>> {
        let mut classes = Vec::new();

        loop {
            classes.push(self.parse_class()?);
            self.expect(Symbol::Semicolon)?;

            if self.try_consume(TokenType::Eof)?.is_some() {
                return Ok(ast::Program { classes });
            }
        }
    }

    fn parse_class(&mut self) -> Result<ast::Class<'a>, ParserError<'a>> {
        self.expect(Symbol::Class)?;
        let name = self.parse_name()?;

        let inherits = match self.try_consume(Symbol::Inherits)? {
            Some(_) => Some(self.parse_name()?),
            _ => None,
        };

        self.expect(Symbol::BraceLeft)?;
        let mut features = Vec::new();

        loop {
            if self.try_consume(Symbol::BraceRight)?.is_some() {
                break;
            }

            features.push(self.parse_feature()?);
        }

        Ok(ast::Class {
            name,
            inherits,
            features,
        })
    }

    fn parse_name(&mut self) -> Result<ast::Name<'a>, ParserError<'a>> {
        // XXX: capitalization is not checked in the parser (might want to revisit this later)
        Ok(ast::Name(self.expect(TokenType::Ident)?))
    }

    fn parse_feature(&mut self) -> Result<ast::Feature<'a>, ParserError<'a>> {
        let name = self.parse_name()?;

        Ok(select!(self: {
            Symbol::ParenLeft => ast::Feature::Method(self.parse_method(name)?),
            Symbol::Colon => ast::Feature::Field(self.parse_binding(name)?),
            _ => @error,
        }))
    }

    fn parse_method(&mut self, name: Name<'a>) -> Result<ast::Method<'a>, ParserError<'a>> {
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

        let return_ty = self.parse_name()?;

        self.expect(Symbol::BraceLeft)?;
        let body = self.parse_expr()?;

        Ok(ast::Method {
            name,
            params,
            return_ty,
            body,
        })
    }

    fn parse_formal(&mut self) -> Result<ast::Formal<'a>, ParserError<'a>> {
        let name = self.parse_name()?;
        self.expect(Symbol::Semicolon)?;
        let ty = self.parse_name()?;

        Ok(ast::Formal { name, ty })
    }

    fn parse_binding(&mut self, name: Name<'a>) -> Result<ast::Binding<'a>, ParserError<'a>> {
        self.expect(Symbol::Semicolon)?;
        let ty = self.parse_name()?;

        let init = if self.try_consume(Symbol::ArrowLeft)?.is_some() {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(ast::Binding { name, ty, init })
    }

    fn parse_expr(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.parse_expr_assign()
    }

    fn parse_expr_assign(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        if self.matches_nth(0, TokenType::Ident) && self.matches_nth(1, Symbol::ArrowLeft) {
            let name = self.parse_name()?;
            self.expect(Symbol::ArrowLeft)?;
            let expr = self.parse_expr_assign()?;

            Ok(Box::new(Expr::Assignment(ast::Assignment { name, expr })))
        } else {
            self.parse_expr_not()
        }
    }

    fn parse_un_op<R, D>(
        &mut self,
        matcher: impl Matcher,
        mut recurse: R,
        mut descend: D,
    ) -> Result<Box<ast::Expr<'a>>, ParserError<'a>>
    where
        R: FnMut(&mut Self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>>,
        D: FnMut(&mut Self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>>,
    {
        if let Some(sym) = self.try_consume(matcher)? {
            let op = match sym.ty() {
                TokenType::Symbol(sym) => UnOpKind::try_from(sym).unwrap(),
                _ => unreachable!(),
            };

            let expr = recurse(self)?;

            Ok(Box::new(Expr::UnOp(ast::UnOpExpr { op, expr })))
        } else {
            descend(self)
        }
    }

    fn parse_expr_not(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.parse_un_op(Symbol::Not, Self::parse_expr_not, Self::parse_expr_cmp)
    }

    fn parse_expr_cmp(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        const CMP_OPS: [Symbol; 3] = [Symbol::LessEquals, Symbol::Less, Symbol::Equals];

        let lhs = self.parse_expr_addsub()?;

        if let Some(sym) = self.try_consume(&CMP_OPS)? {
            let op = match sym.ty() {
                TokenType::Symbol(sym) => BinOpKind::try_from(sym).unwrap(),
                _ => unreachable!(),
            };

            let rhs = self.parse_expr_addsub()?;

            Ok(Box::new(Expr::BinOp(ast::BinOpExpr { op, lhs, rhs })))
        } else {
            Ok(lhs)
        }
    }

    fn parse_bin_op_lassoc<D>(
        &mut self,
        matcher: impl Matcher + Copy,
        mut descend: D,
    ) -> Result<Box<ast::Expr<'a>>, ParserError<'a>>
    where
        D: FnMut(&mut Self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>>,
    {
        let mut lhs = descend(self)?;

        while let Some(sym) = self.try_consume(matcher)? {
            let op = match sym.ty() {
                TokenType::Symbol(sym) => BinOpKind::try_from(sym).unwrap(),
                _ => unreachable!(),
            };

            let rhs = descend(self)?;
            lhs = Box::new(Expr::BinOp(ast::BinOpExpr { op, lhs, rhs }));
        }

        Ok(lhs)
    }

    fn parse_expr_addsub(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.parse_bin_op_lassoc(&[Symbol::Plus, Symbol::Minus], Self::parse_expr_muldiv)
    }

    fn parse_expr_muldiv(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.parse_bin_op_lassoc(&[Symbol::Asterisk, Symbol::Slash], Self::parse_expr_is_void)
    }

    fn parse_expr_is_void(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.parse_un_op(Symbol::IsVoid, Self::parse_expr_is_void, Self::parse_expr_compl)
    }

    fn parse_expr_compl(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.parse_un_op(Symbol::Tilde, Self::parse_expr_compl, Self::parse_expr_static_dispatch)
    }

    fn parse_expr_static_dispatch(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        let mut object = self.parse_expr_dynamic_dispatch()?;

        while self.try_consume(Symbol::At)?.is_some() {
            let ty = self.parse_name()?;
            self.expect(Symbol::Dot)?;
            object = self.parse_method_call(ast::Receiver::Static { object, ty })?;
        }

        Ok(object)
    }

    fn parse_method_call(
        &mut self,
        receiver: ast::Receiver<'a>,
    ) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        let method = self.parse_name()?;
        self.expect(Symbol::ParenLeft)?;
        let mut args = Vec::new();

        while self.try_consume(Symbol::ParenRight)?.is_none() {
            if !args.is_empty() {
                self.expect(Symbol::Comma)?;
            }

            args.push(self.parse_expr()?);
        }

        Ok(Box::new(Expr::Call(ast::Call {
            receiver,
            method,
            args,
        })))
    }

    fn parse_expr_dynamic_dispatch(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        let mut object = self.parse_expr_atom()?;

        while self.try_consume(Symbol::Dot)?.is_some() {
            object = self.parse_method_call(ast::Receiver::Dynamic(object))?;
        }

        Ok(object)
    }

    fn parse_expr_atom(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
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

    fn parse_if(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.expect(Symbol::If)?;
        let antecedent = self.parse_expr()?;
        self.expect(Symbol::Then)?;
        let consequent = self.parse_expr()?;
        self.expect(Symbol::Else)?;
        let alternative = self.parse_expr()?;
        self.expect(Symbol::Fi)?;

        Ok(Box::new(Expr::If(ast::If {
            antecedent,
            consequent,
            alternative,
        })))
    }

    fn parse_while(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.expect(Symbol::While)?;
        let condition = self.parse_expr()?;
        self.expect(Symbol::Loop)?;
        let body = self.parse_expr()?;
        self.expect(Symbol::Pool)?;

        Ok(Box::new(Expr::While(ast::While { condition, body })))
    }

    fn parse_block(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.expect(Symbol::BraceLeft)?;
        let mut body = Vec::new();

        loop {
            body.push(self.parse_expr()?);
            self.expect(Symbol::Semicolon)?;

            if self.try_consume(Symbol::BraceRight)?.is_none() {
                break;
            }
        }

        Ok(Box::new(Expr::Block(ast::Block { body })))
    }

    fn parse_let(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.expect(Symbol::Let)?;
        let mut bindings = Vec::new();

        loop {
            let name = self.parse_name()?;
            bindings.push(self.parse_binding(name)?);

            if self.try_consume(Symbol::In)?.is_none() {
                break;
            } else {
                self.expect(Symbol::Comma)?;
            }
        }

        let expr = self.parse_expr()?;

        Ok(Box::new(Expr::Let(ast::Let { bindings, expr })))
    }

    fn parse_case(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.expect(Symbol::Case)?;
        let scrutinee = self.parse_expr()?;
        self.expect(Symbol::Of)?;
        let mut arms = Vec::new();

        loop {
            let name = self.parse_name()?;
            self.expect(Symbol::Colon)?;
            let ty = self.parse_name()?;
            self.expect(Symbol::Implies)?;
            let expr = self.parse_expr()?;
            self.expect(Symbol::Semicolon)?;

            arms.push(ast::CaseArm { name, ty, expr });

            if self.try_consume(Symbol::Esac)?.is_none() {
                break;
            }
        }

        Ok(Box::new(Expr::Case(ast::Case { scrutinee, arms })))
    }

    fn parse_new(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.expect(Symbol::New)?;

        Ok(Box::new(Expr::New(ast::New(self.parse_name()?))))
    }

    fn parse_paren(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        self.expect(Symbol::ParenLeft)?;
        let expr = self.parse_expr()?;
        self.expect(Symbol::ParenRight)?;

        Ok(expr)
    }

    fn parse_expr_self_call(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        if self.matches_nth(1, Symbol::ParenLeft) {
            self.parse_method_call(ast::Receiver::SelfType)
        } else {
            Ok(Box::new(Expr::Name(self.parse_name()?)))
        }
    }

    fn parse_int_lit(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        let token = self.expect(TokenType::Int)?;

        Ok(Box::new(Expr::Int(ast::IntLit(token))))
    }

    fn parse_string_lit(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        let token = self.expect(TokenType::String)?;

        Ok(Box::new(Expr::String(ast::StringLit(token))))
    }

    fn parse_bool_lit(&mut self) -> Result<Box<ast::Expr<'a>>, ParserError<'a>> {
        let token = self.expect(&[Symbol::True, Symbol::False])?;

        Ok(Box::new(Expr::Bool(ast::BoolLit(token))))
    }
}
