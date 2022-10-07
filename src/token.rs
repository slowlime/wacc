use std::borrow::Cow;

use itertools::Itertools;
use once_cell::sync::OnceCell;
use phf::phf_map;

use crate::position::Span;

pub struct Token<'a> {
    pub span: Span,
    pub value: TokenValue<'a>,
}

pub enum TokenType {
    Int,
    Symbol,
    Ident,
    TypeIdent,
    String,
    Eof,
}

pub enum TokenValue<'a> {
    Int(i32),
    Symbol(Symbol),
    Ident(&'a [u8]),
    String(Cow<'a, [u8]>),
    Eof,
}

impl TokenValue<'_> {
    pub fn ty(&self) -> TokenType {
        match self {
            Self::Int(_) => TokenType::Int,
            Self::Symbol(_) => TokenType::Symbol,
            Self::Ident(_) => TokenType::Ident,
            Self::String(_) => TokenType::String,
            Self::Eof => TokenType::Eof,
        }
    }
}

macro_rules! symbols {
    ($( $lit:literal => $variant:ident ),+,) => { symbols!($( $lit => $variant ),+); };

    ($( $lit:literal => $variant:ident ),+) => {
        #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
        pub enum Symbol {
            $( $variant ),+
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
                $( $lit => Self::$variant ),+
            };

            const MAX_LENGTH: usize = max_arr([$( $lit.len() ),+]);

            fn get_prefix_lengths() -> &'static [usize] {
                static PREFIX_LENGTHS: OnceCell<Vec<usize>> = OnceCell::new();

                PREFIX_LENGTHS.get_or_init(|| {
                    let mut lengths = [$( $lit.len() ),+];
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

            pub fn as_str(&self) -> &'static [u8] {
                match self {
                    $( Self::$variant => $lit, )+
                }
            }
        }
    };
}

symbols! {
    // keywords
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

    // symbolic operators
    b"+" => Plus,
    b"-" => Minus,
    b"*" => Asterisk,
    b"/" => Slash,
    b"~" => Tilde,
    b"<" => Less,
    b"<=" => LessEquals,
    b"=" => Equals,

    // punctuation
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
