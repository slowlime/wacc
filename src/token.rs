use std::borrow::Cow;
use std::fmt::Display;
use std::io::{self, Write};

use itertools::Itertools;
use once_cell::sync::OnceCell;
use phf::phf_map;

use crate::position::{Span, Spanned};
use crate::try_match;

pub const BACKSPACE: u8 = 8;
pub const VERTICAL_TAB: u8 = 11;
pub const FORM_FEED: u8 = 12;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token<'buf> {
    pub span: Span,
    pub value: TokenValue<'buf>,
}

impl Token<'_> {
    pub fn ty(&self) -> TokenType {
        self.value.ty()
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    Int,
    Symbol(Symbol),
    Ident,
    String,
    Eof,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Int => "integer",
                Self::Symbol(sym) => sym.as_str(),
                Self::Ident => "identifier",
                Self::String => "string",
                Self::Eof => "end of file",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenValue<'buf> {
    Int(i32),
    Symbol(Symbol),
    Ident(&'buf [u8]),
    String(Cow<'buf, [u8]>),
    Eof,
}

impl TokenValue<'_> {
    pub fn ty(&self) -> TokenType {
        match self {
            Self::Int(_) => TokenType::Int,
            Self::Symbol(sym) => TokenType::Symbol(*sym),
            Self::Ident(_) => TokenType::Ident,
            Self::String(_) => TokenType::String,
            Self::Eof => TokenType::Eof,
        }
    }
}

impl<'buf> TryFrom<Token<'buf>> for Spanned<i32> {
    type Error = ();

    fn try_from(token: Token<'buf>) -> Result<Self, Self::Error> {
        Ok(Spanned {
            span: token.span,
            value: try_match!(token.value, TokenValue::Int(i) => i).ok_or(())?,
        })
    }
}

impl<'buf> TryFrom<Token<'buf>> for Spanned<Symbol> {
    type Error = ();

    fn try_from(token: Token<'buf>) -> Result<Self, Self::Error> {
        Ok(Spanned {
            span: token.span,
            value: try_match!(token.value, TokenValue::Symbol(sym) => sym).ok_or(())?,
        })
    }
}

impl<'buf> TryFrom<Token<'buf>> for Spanned<&'buf [u8]> {
    type Error = ();

    fn try_from(token: Token<'buf>) -> Result<Self, Self::Error> {
        Ok(Spanned {
            span: token.span,
            value: match token.value {
                TokenValue::Ident(id) => id,
                TokenValue::String(Cow::Borrowed(s)) => s,
                _ => return Err(()),
            },
        })
    }
}

impl<'buf> TryFrom<Token<'buf>> for Spanned<Cow<'buf, [u8]>> {
    type Error = ();

    fn try_from(token: Token<'buf>) -> Result<Self, Self::Error> {
        Ok(Spanned {
            span: token.span,
            value: match token.value {
                TokenValue::Ident(id) => id.into(),
                TokenValue::String(s) => s,
                _ => return Err(()),
            },
        })
    }
}

impl<'buf> TryFrom<Token<'buf>> for Spanned<bool> {
    type Error = ();

    fn try_from(token: Token<'buf>) -> Result<Self, Self::Error> {
        Ok(Spanned {
            span: token.span,
            value: match token.value {
                TokenValue::Symbol(Symbol::True) => true,
                TokenValue::Symbol(Symbol::False) => false,
                _ => return Err(()),
            },
        })
    }
}

macro_rules! symbols {
    ($( $cat:ident : { $( $lit:literal => $variant:ident ),+ $(,)? } )+) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Symbol {
            $( $( $variant, )+ )+
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum SymbolCategory {
            $( $cat, )+
        }

        const fn max_arr<const N: usize>(values: [usize; N]) -> usize {
            const fn max_arr_impl<const N: usize>(acc: usize, idx: usize, values: [usize; N]) -> usize {
                if idx >= N {
                    acc
                } else {
                    let x = values[idx];

                    max_arr_impl(if x > acc { x } else { acc }, idx + 1, values)
                }
            }

            max_arr_impl(0, 0, values)
        }

        impl Symbol {
            const SYMBOLS: phf::Map<&'static [u8], Symbol> = phf_map! {
                $( $( $lit => Self::$variant, )+ )+
            };

            const MAX_LENGTH: usize = max_arr([$( $( $lit.len(), )+ )+]);

            fn get_prefix_lengths() -> &'static [usize] {
                static PREFIX_LENGTHS: OnceCell<Vec<usize>> = OnceCell::new();

                PREFIX_LENGTHS.get_or_init(|| {
                    let mut lengths = [$( $( $lit.len(), )+ )+];
                    lengths.sort_unstable();
                    lengths.into_iter().rev().dedup().collect()
                })
            }

            /// Tries to parse the beginning of `input` as a symbol.
            pub fn parse_prefix(input: &[u8]) -> Option<Symbol> {
                let mut buf = [0u8; Self::MAX_LENGTH];
                let len = buf.len().min(input.len());
                let buf = &mut buf[..len];
                buf.copy_from_slice(&input[..len]);

                for i in 0..len {
                    buf[i] = buf[i].to_ascii_lowercase();
                }

                Self::get_prefix_lengths()
                    .iter()
                    .filter_map(|&len| buf.get(0..len))
                    .find_map(|prefix| Self::SYMBOLS.get(prefix))
                    .copied()
            }

            pub fn as_slice(&self) -> &'static [u8] {
                match self {
                    $( $( Self::$variant => $lit, )+ )+
                }
            }

            pub fn as_str(&self) -> &'static str {
                std::str::from_utf8(self.as_slice()).unwrap()
            }

            pub fn category(&self) -> SymbolCategory {
                match self {
                    $( $( Self::$variant => SymbolCategory::$cat, )+ )+
                }
            }
        }
    };
}

symbols! {
    Keyword: {
        b"class" => Class,
        b"else" => Else,
        b"false" => False,
        b"fi" => Fi,
        b"if" => If,
        b"in" => In,
        b"inherits" => Inherits,
        b"isvoid" => IsVoid,
        b"let" => Let,
        b"loop" => Loop,
        b"pool" => Pool,
        b"then" => Then,
        b"while" => While,
        b"case" => Case,
        b"esac" => Esac,
        b"new" => New,
        b"of" => Of,
        b"not" => Not,
        b"true" => True,
    }

    Operator: {
        b"+" => Plus,
        b"-" => Minus,
        b"*" => Asterisk,
        b"/" => Slash,
        b"~" => Tilde,
        b"<" => Less,
        b"<=" => LessEquals,
        b"=" => Equals,
    }

    Punctuation: {
        b"{" => BraceLeft,
        b"}" => BraceRight,
        b"(" => ParenLeft,
        b")" => ParenRight,
        b":" => Colon,
        b"," => Comma,
        b";" => Semicolon,
        b"<-" => ArrowLeft,
        b"@" => At,
        b"." => Dot,
        b"=>" => Implies,
    }
}

pub fn write_escaped_string(s: &[u8], mut out: impl Write) -> io::Result<()> {
    write!(out, "\"")?;

    for c in s {
        match *c {
            b'\\' => write!(out, "\\\\")?,
            b'"' => write!(out, "\\\"")?,
            b'\n' => write!(out, "\\n")?,
            b'\t' => write!(out, "\\t")?,
            BACKSPACE => write!(out, "\\b")?,
            FORM_FEED => write!(out, "\\f")?,
            0x20..=0x7e => out.write_all(&[*c])?,
            _ => write!(out, "\\{:03o}", c)?,
        }
    }

    write!(out, "\"")?;

    Ok(())
}
