use crate::cursor::Cursor;
use crate::position::{Position, Span};
use crate::token::{Token, TokenValue};

type ScanResult<'a> = Result<TokenValue<'a>, PosLexerError>;

fn is_whitespace(c: u8) -> bool {
    const FORM_FEED: u8 = 12;
    const VERTICAL_TAB: u8 = 11;

    matches!(c, b' ' | b'\n' | FORM_FEED | b'\r' | b'\t' | VERTICAL_TAB)
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LexerErrorKind {}

#[derive(Debug, Clone, Eq, PartialEq)]
struct PosLexerError {
    end: Position,
    kind: LexerErrorKind,
}

impl PosLexerError {
    fn with_start(self, start: Position) -> LexerError {
        LexerError {
            span: Span {
                start,
                end: self.end,
            },
            kind: self.kind,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LexerError {
    span: Span,
    kind: LexerErrorKind,
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(cursor: Cursor<'a>) -> Self {
        Self { cursor, eof: false }
    }

    fn scan_int(&mut self) -> ScanResult<'a> {
        todo!()
    }

    fn scan_symbol(&mut self) -> ScanResult<'a> {
        todo!()
    }

    fn scan_string(&mut self) -> ScanResult<'a> {
        todo!()
    }

    fn skip_comment(&mut self) -> Result<(), PosLexerError> {
        todo!()
    }

    fn skip_block_comment(&mut self) -> Result<(), PosLexerError> {
        todo!()
    }

    fn skip_whitespace(&mut self) -> Result<(), PosLexerError> {
        todo!()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.cursor.pos();

        let scan_result = loop {
            break match self.cursor.peek() {
                None if self.eof => return None,

                None => {
                    self.eof = true;

                    return Some(Ok(Token {
                        value: TokenValue::Eof,
                        span: Span { start, end: start },
                    }));
                }

                Some(b'"') => self.scan_string(),

                Some(b'-') if self.cursor.starts_with(b"--") => match self.skip_comment() {
                    Ok(()) => continue,
                    Err(e) => break Err(e),
                },

                Some(b'(') if self.cursor.starts_with(b"(*") => match self.skip_block_comment() {
                    Ok(()) => continue,
                    Err(e) => break Err(e),
                },

                Some(c) if is_whitespace(c) => match self.skip_whitespace() {
                    Ok(()) => continue,
                    Err(e) => break Err(e),
                }

                Some(c) if c.is_ascii_digit() => self.scan_int(),

                Some(_) => self.scan_symbol(),
            };
        };

        Some(match scan_result {
            Ok(value) => Ok(Token {
                span: Span {
                    start,
                    end: self.cursor.prev_pos(),
                },
                value,
            }),

            Err(err) => Err(err.with_start(start)),
        })
    }
}
