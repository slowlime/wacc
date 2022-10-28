use std::io::{self, Write};
use std::path::{Path, PathBuf};

use clap::{Args, Parser as ClapParser, Subcommand, ValueEnum};
use color_eyre::eyre::{eyre, Context, Report};

use crate::cursor::Cursor;
use crate::lexer::{Lexer, LexerError};
use crate::source::{Source, SourceBuffer};
use crate::token::{self, write_escaped_string, Symbol, SymbolCategory, Token, TokenValue};

#[derive(ClapParser, Debug, Clone)]
#[command(version)]
pub struct WaccCli {
    #[command(subcommand)]
    pub command: WaccCommand,
}

#[derive(Subcommand, Debug, Clone)]
pub enum WaccCommand {
    /// Dump the debug representation of input at a particular compilation stage
    Dump(DumpArgs),
}

#[derive(Args, Debug, Clone)]
pub struct DumpArgs {
    /// The input files
    pub paths: Vec<PathBuf>,

    /// The compiler stage to stop at
    #[arg(short, long, value_enum)]
    pub stage: DumpStage,

    /// The output format to use
    #[arg(short, long, value_enum, default_value_t = DumpFormat::Coolc)]
    pub format: DumpFormat,
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DumpStage {
    Lexer,
    Parser,
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DumpFormat {
    Coolc,
    Debug,
}

impl WaccCli {
    pub fn run(self) -> Result<(), Report> {
        match self.command {
            WaccCommand::Dump(test_args) => DumpRunner::new(test_args).run(),
        }
    }
}

struct DumpRunner {
    paths: Vec<PathBuf>,
    stage: DumpStage,
    format: DumpFormat,
}

impl DumpRunner {
    pub fn new(DumpArgs { paths, stage, format }: DumpArgs) -> Self {
        Self { paths, stage, format }
    }

    pub fn run(self) -> Result<(), Report> {
        match self.stage {
            DumpStage::Lexer => self.test_lexer(),
            DumpStage::Parser => self.test_parser(),
        }
    }

    fn test_lexer(self) -> Result<(), Report> {
        let paths = &self.paths;
        let mut buf = SourceBuffer::new();
        let mut source = Source::new(&mut buf);

        for path in paths {
            let id = source
                .load(path.clone())
                .wrap_err_with(|| eyre!("Could not load the file {}", path.display()))?;
            let src_file = source.get(id).unwrap();
            let cursor = Cursor::new(src_file);
            let tokens = Lexer::new(cursor);

            Self::dump_tokens(self.format, &path, tokens, io::stdout().lock())
                .wrap_err("Could not dump tokens to stdout")?;
        }

        Ok(())
    }

    fn dump_tokens<'src, I>(
        format: DumpFormat,
        path: &Path,
        tokens: I,
        mut out: impl Write,
    ) -> io::Result<()>
    where
        I: Iterator<Item = Result<Token<'src>, LexerError>>,
    {
        match format {
            DumpFormat::Coolc => Self::dump_tokens_coolc(path, tokens, out),
            DumpFormat::Debug => write!(out, "{:#?}", tokens.collect::<Vec<_>>()),
        }
    }

    fn dump_tokens_coolc<'src, I>(path: &Path, tokens: I, mut out: impl Write) -> io::Result<()>
    where
        I: Iterator<Item = Result<Token<'src>, LexerError>>,
    {
        let name = format!("{}", path.display());
        write!(out, "#name ")?;
        write_escaped_string(name.as_bytes(), &mut out)?;
        writeln!(out)?;

        for token in tokens {
            let token = match token {
                Ok(Token { value: TokenValue::Eof, .. }) => continue,

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
                    if id.get(0).map(u8::is_ascii_uppercase).unwrap_or(false) {
                        out.write(b"TYPEID ")?;
                    } else {
                        out.write(b"OBJECTID ")?;
                    }

                    out.write(id)?;
                }

                TokenValue::Symbol(sym) => match sym.category() {
                    // booleans enjoy special treatment by the reference lexer
                    _ if [Symbol::True, Symbol::False].contains(&sym) => write!(
                        out,
                        "BOOL_CONST {}",
                        if sym == Symbol::True { "true" } else { "false" }
                    )?,

                    SymbolCategory::Keyword => {
                        write!(out, "{}", sym.as_str().to_ascii_uppercase())?
                    }

                    // weird edge-cases (since they're multibyte, I guess?)
                    _ if sym == Symbol::ArrowLeft => write!(out, "ASSIGN")?,
                    _ if sym == Symbol::Implies => write!(out, "DARROW")?,
                    _ if sym == Symbol::LessEquals => write!(out, "LE")?,

                    _ => write!(out, "'{}'", sym.as_str())?,
                },

                TokenValue::String(s) => {
                    write!(out, "STR_CONST ")?;
                    token::write_escaped_string(&s, &mut out)?;
                }
            }

            writeln!(out)?;
        }

        Ok(())
    }

    fn test_parser(self) -> Result<(), Report> {
        todo!()
    }
}
