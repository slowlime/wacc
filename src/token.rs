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
    TypeIdent(&'a [u8]),
    String(Cow<'a, [u8]>),
    Eof,
}

impl TokenValue<'_> {
    pub fn ty(&self) -> TokenType {
        match self {
            Self::Int(_) => TokenType::Int,
            Self::Symbol(_) => TokenType::Symbol,
            Self::Ident(_) => TokenType::Ident,
            Self::TypeIdent(_) => TokenType::TypeIdent,
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

        impl Symbol {
            const SYMBOLS: phf::Map<&'static [u8], Symbol> = phf_map! {
                $( $lit => Self::$variant ),+
            };

            fn get_prefix_lengths() -> &'static [usize] {
                static PREFIX_LENGTHS: OnceCell<Vec<usize>> = OnceCell::new();

                PREFIX_LENGTHS.get_or_init(|| {
                    let mut lengths = [$( $lit.len() ),+];
                    lengths.sort_unstable();
                    lengths.into_iter().rev().dedup().collect()
                })
            }

            pub fn parse_prefix(input: &[u8]) -> Option<Symbol> {
                Self::get_prefix_lengths()
                    .iter()
                    .filter_map(|&len| input.get(0..len))
                    .find_map(|prefix| Self::SYMBOLS.get(prefix))
                    .copied()
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
}
