use std::borrow::Cow;
use std::error::Error;
use std::fmt::Display;
use std::iter::FusedIterator;
use std::num::{IntErrorKind, ParseIntError};

use serde::Serialize;

use crate::parse::cursor::Cursor;
use crate::parse::token::{Symbol, Token, TokenValue, BACKSPACE, FORM_FEED, VERTICAL_TAB};
use crate::position::{HasSpan, Position, Span};
use crate::try_match;

type ScanResult<'buf> = Result<TokenValue<'buf>, PosLexerError>;

fn is_whitespace(c: u8) -> bool {
    matches!(c, b' ' | b'\n' | FORM_FEED | b'\r' | b'\t' | VERTICAL_TAB)
}

fn is_ident_start(c: u8) -> bool {
    c.is_ascii_alphabetic()
}

fn is_ident_continuation(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}

fn scan_symbol(s: &[u8], exact: bool) -> Option<TokenValue<'_>> {
    let f = if exact {
        Symbol::parse_exact
    } else {
        Symbol::parse_prefix
    };

    match f(s) {
        // boolean literals must start with a lowercase letter
        Some(Symbol::True | Symbol::False) if !s[0].is_ascii_lowercase() => None,

        Some(sym) => Some(TokenValue::Symbol(sym)),

        None => None,
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum StringCharacterKind {
    Normal,
    Backslash,
    Escaped,
}

fn build_escape_processor<F, R>(mut f: F) -> impl FnMut(&u8) -> R
where
    F: FnMut(u8, StringCharacterKind, usize) -> R,
{
    let mut escaped = false;
    let mut byte_idx = 0;

    move |&c| {
        let result = match c {
            _ if escaped => {
                escaped = false;

                f(c, StringCharacterKind::Escaped, byte_idx)
            }

            b'\\' => {
                escaped = true;

                f(c, StringCharacterKind::Backslash, byte_idx)
            }

            _ => f(c, StringCharacterKind::Normal, byte_idx),
        };

        byte_idx += 1;

        result
    }
}

fn process_escapes(input: &[u8]) -> Vec<u8> {
    input
        .iter()
        .filter_map(build_escape_processor(|c, kind, _| match kind {
            StringCharacterKind::Normal => Some(c),
            StringCharacterKind::Backslash => None,

            StringCharacterKind::Escaped => Some(match c {
                b'b' => BACKSPACE,
                b't' => b'\t',
                b'n' => b'\n',
                b'f' => FORM_FEED,
                _ => c,
            }),
        }))
        .collect()
}

#[derive(Serialize, Debug, Clone, Copy, Eq, PartialEq)]
pub enum LexerErrorKind {
    NumberTooLarge,
    UnterminatedComment,
    UnterminatedString,
    UnrecognizedCharacter(u8),
}

impl From<ParseIntError> for LexerErrorKind {
    fn from(err: ParseIntError) -> Self {
        match err.kind() {
            IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => Self::NumberTooLarge,
            _ => unimplemented!(),
        }
    }
}

impl Display for LexerErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Self::UnrecognizedCharacter(c) = self {
            write!(f, "encountered an unrecognized character '")?;

            if c.is_ascii_graphic() {
                write!(f, "{}", *c as char)?;
            } else {
                write!(f, "\\x{:02x}", c)?;
            }

            return write!(f, "'");
        }

        write!(
            f,
            "{}",
            match self {
                Self::NumberTooLarge => "the number literal is too large",
                Self::UnterminatedComment => "the block comment is not terminated",
                Self::UnterminatedString => "the string is not terminated",

                Self::UnrecognizedCharacter(_) => unreachable!(),
            }
        )
    }
}

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

#[derive(Serialize, Debug, Clone, Eq, PartialEq)]
pub struct LexerError {
    span: Span,
    kind: LexerErrorKind,
}

impl LexerError {
    pub fn kind(&self) -> LexerErrorKind {
        self.kind
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "lexical analysis failed: {}", self.kind)
    }
}

impl Error for LexerError {}

impl HasSpan for LexerError {
    fn span(&self) -> Cow<'_, Span> {
        Cow::Borrowed(&self.span)
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'buf> {
    cursor: Cursor<'buf>,
    eof: bool,
}

impl<'buf> Lexer<'buf> {
    pub fn new(cursor: Cursor<'buf>) -> Self {
        Self { cursor, eof: false }
    }

    fn create_error_at_pos(&self, kind: LexerErrorKind) -> PosLexerError {
        PosLexerError {
            end: self.cursor.pos(),
            kind,
        }
    }

    fn create_error_behind(&self, kind: LexerErrorKind) -> PosLexerError {
        PosLexerError {
            end: self.cursor.prev_pos(),
            kind,
        }
    }

    fn scan_int(&mut self) -> ScanResult<'buf> {
        let digits = self.cursor.consume_while(u8::is_ascii_digit);
        let value = std::str::from_utf8(digits)
            .unwrap()
            .parse::<i32>()
            .map_err(|e| self.create_error_behind(e.into()))?;

        Ok(TokenValue::Int(value))
    }

    fn scan_ident_or_symbol(&mut self) -> ScanResult<'buf> {
        let ident = self.cursor.consume_while(|&c| is_ident_continuation(c));

        Ok(scan_symbol(ident, true).unwrap_or(TokenValue::Ident(Cow::Borrowed(ident))))
    }

    fn scan_string(&mut self) -> ScanResult<'buf> {
        self.cursor.consume_expecting(b"\"").unwrap();

        let mut cursor_at_start = self.cursor.clone();
        let mut has_escapes = false;
        let mut invalid_char = None;

        let value = self
            .cursor
            .consume_while(build_escape_processor(|c, kind, idx| match (kind, c) {
                (StringCharacterKind::Normal, b'\n') | (_, b'\0') => {
                    invalid_char = invalid_char.or(Some((idx, c)));

                    // terminate at \n
                    c != b'\n'
                }

                (StringCharacterKind::Normal, b'"') => false,
                (StringCharacterKind::Normal, _) => true,
                (StringCharacterKind::Escaped, _) => true,

                (StringCharacterKind::Backslash, _) => {
                    has_escapes = true;

                    true
                }
            }));

        self.cursor
            .consume_expecting(b"\"")
            .ok_or_else(|| self.create_error_at_pos(LexerErrorKind::UnterminatedString))?;

        if let Some((idx, c)) = invalid_char {
            cursor_at_start.consume_n(idx);

            return Err(PosLexerError {
                end: cursor_at_start.pos(),
                kind: LexerErrorKind::UnrecognizedCharacter(c),
            });
        }

        let value = if has_escapes {
            process_escapes(value).into()
        } else {
            value.into()
        };

        Ok(TokenValue::String(value))
    }

    fn skip_comment(&mut self) {
        self.cursor.consume_expecting(b"--").unwrap();
        self.cursor.consume_while(|&c| c != b'\n');
        self.cursor.next();
    }

    fn skip_block_comment(&mut self) -> Result<(), PosLexerError> {
        self.cursor.consume_expecting(b"(*").unwrap();
        let mut depth = 1;

        loop {
            let c = self
                .cursor
                .next()
                .ok_or_else(|| self.create_error_at_pos(LexerErrorKind::UnterminatedComment))?;

            match c {
                b'(' if self.cursor.consume_expecting(b"*").is_some() => depth += 1,
                b'*' if self.cursor.consume_expecting(b")").is_some() => depth -= 1,
                _ => {}
            }

            if depth == 0 {
                return Ok(());
            }
        }
    }

    fn skip_whitespace(&mut self) {
        self.cursor.consume_while(|&c| is_whitespace(c));
    }
}

impl<'buf> Iterator for Lexer<'buf> {
    type Item = Result<Token<'buf>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            return None;
        }

        let mut start;

        let scan_result = loop {
            start = self.cursor.pos();

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

                Some(b'-') if self.cursor.starts_with(b"--") => {
                    self.skip_comment();

                    continue;
                }

                Some(b'(') if self.cursor.starts_with(b"(*") => match self.skip_block_comment() {
                    Ok(()) => continue,
                    Err(e) => break Err(e),
                },

                Some(c) if is_whitespace(c) => {
                    self.skip_whitespace();

                    continue;
                }

                Some(c) if c.is_ascii_digit() => self.scan_int(),

                Some(c) if is_ident_start(c) => self.scan_ident_or_symbol(),

                Some(c) => match scan_symbol(self.cursor.remaining(), false) {
                    Some(value) => {
                        let n =
                            try_match!(value, TokenValue::Symbol(s) => s.as_slice().len()).unwrap();
                        self.cursor.consume_n(n);

                        Ok(value)
                    }

                    None => Err(self.create_error_at_pos(LexerErrorKind::UnrecognizedCharacter(c))),
                },
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

            Err(err) => {
                self.eof = true;

                Err(err.with_start(start))
            }
        })
    }
}

impl<'buf> FusedIterator for Lexer<'buf> {}
