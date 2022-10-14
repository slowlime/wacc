use std::borrow::Cow;
use std::iter::Peekable;

use crate::ast;
use crate::lexer::{Lexer, LexerError};
use crate::token::{Token, TokenType};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParserError<'a> {
    UnexpectedToken {
        expected: Cow<'static, [TokenType]>,
        actual: Token<'a>,
    },

    LexerError(LexerError),
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<ast::Program<'a>, ParserError<'a>> {
        todo!()
    }
}
