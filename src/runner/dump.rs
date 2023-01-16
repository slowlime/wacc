use std::io::{self, Write};

use wacc::parse::token::{write_escaped_string, Symbol, SymbolCategory, Token, TokenValue};
use wacc::parse::LexerError;
use wacc::position::HasSpan;
use wacc::source::Source;

use super::config::LexerOutputFormat;

pub use wacc::ast::dump::{AstDumpError, AstDumpFormat, dump_ast};

pub fn dump_tokens<'buf, I>(
    format: LexerOutputFormat,
    source: &Source<'buf>,
    tokens: I,
    mut out: impl Write,
) -> io::Result<()>
where
    I: Iterator<Item = Result<Token<'buf>, LexerError>>,
{
    match format {
        LexerOutputFormat::Coolc => dump_tokens_coolc(source, tokens, out)?,
        LexerOutputFormat::Debug => writeln!(out, "{:#?}", tokens.collect::<Vec<_>>())?,
    }

    Ok(())
}

fn dump_tokens_coolc<'buf, I>(
    source: &Source<'buf>,
    tokens: I,
    mut out: impl Write,
) -> io::Result<()>
where
    I: Iterator<Item = Result<Token<'buf>, LexerError>>,
{
    let mut peekable = tokens.peekable();
    let src_id = peekable
        .peek()
        .expect("the lexer always returns at least one token")
        .as_ref()
        .map(|token| token.span.start.src)
        .unwrap_or_else(|err| err.span().start.src);
    let name = match src_id {
        Some(src_id) => format!(
            "{}",
            source
                .get(src_id)
                .expect("the span has a valid source id")
                .path()
                .display()
        ),
        None => "<unknown>".to_owned(),
    };
    write!(out, "#name ")?;
    write_escaped_string(name.as_bytes(), &mut out)?;
    writeln!(out)?;

    for token in peekable {
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

                out.write_all(&id)?;
            }

            TokenValue::Symbol(sym) => match sym.category() {
                // booleans enjoy special treatment by the reference lexer
                _ if [Symbol::True, Symbol::False].contains(&sym) => write!(
                    out,
                    "BOOL_CONST {}",
                    if sym == Symbol::True { "true" } else { "false" }
                )?,

                SymbolCategory::Keyword => write!(out, "{}", sym.as_str().to_ascii_uppercase())?,

                // weird edge-cases (since they're multibyte, I guess?)
                _ if sym == Symbol::ArrowLeft => write!(out, "ASSIGN")?,
                _ if sym == Symbol::Implies => write!(out, "DARROW")?,
                _ if sym == Symbol::LessEquals => write!(out, "LE")?,

                _ => write!(out, "'{}'", sym.as_str())?,
            },

            TokenValue::String(s) => {
                write!(out, "STR_CONST ")?;
                write_escaped_string(&s, &mut out)?;
            }
        }

        writeln!(out)?;
    }

    Ok(())
}
