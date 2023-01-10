use std::path::PathBuf;

use clap::{arg, command, value_parser, ValueEnum};

#[derive(Debug, Clone)]
pub struct WaccConfig {
    pub paths: Vec<PathBuf>,
    pub output: OutputKind,
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CompilationStage {
    Lexer,
    Parser,
    Typeck,
    Codegen,
}

impl Default for CompilationStage {
    fn default() -> Self {
        Self::Codegen
    }
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OutputFormat {
    Coolc,
    Debug,
    Wasm,
    Wat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LexerOutputFormat {
    Coolc,
    Debug,
}

impl TryFrom<OutputFormat> for LexerOutputFormat {
    type Error = &'static str;

    fn try_from(format: OutputFormat) -> Result<LexerOutputFormat, Self::Error> {
        match format {
            OutputFormat::Coolc => Ok(Self::Coolc),
            OutputFormat::Debug => Ok(Self::Debug),
            _ => Err("this format cannot be used for the current compilation stage"),
        }
    }
}

impl Default for LexerOutputFormat {
    fn default() -> Self {
        Self::Coolc
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParserOutputFormat {
    Coolc,
    Debug,
}

impl TryFrom<OutputFormat> for ParserOutputFormat {
    type Error = &'static str;

    fn try_from(format: OutputFormat) -> Result<ParserOutputFormat, Self::Error> {
        match format {
            OutputFormat::Coolc => Ok(Self::Coolc),
            OutputFormat::Debug => Ok(Self::Debug),
            _ => Err("this format cannot be used for the current compilation stage"),
        }
    }
}

impl Default for ParserOutputFormat {
    fn default() -> Self {
        Self::Coolc
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeckOutputFormat {
    Coolc,
    Debug,
}

impl TryFrom<OutputFormat> for TypeckOutputFormat {
    type Error = &'static str;

    fn try_from(format: OutputFormat) -> Result<TypeckOutputFormat, Self::Error> {
        match format {
            OutputFormat::Coolc => Ok(Self::Coolc),
            OutputFormat::Debug => Ok(Self::Debug),
            _ => Err("this format cannot be used for the current compilation stage"),
        }
    }
}

impl Default for TypeckOutputFormat {
    fn default() -> Self {
        Self::Coolc
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CodegenOutputFormat {
    Debug,
    #[allow(dead_code)]
    Wat,
    Wasm,
}

impl TryFrom<OutputFormat> for CodegenOutputFormat {
    type Error = &'static str;

    fn try_from(format: OutputFormat) -> Result<CodegenOutputFormat, Self::Error> {
        match format {
            OutputFormat::Debug => Ok(Self::Debug),
            OutputFormat::Wasm => Ok(Self::Wasm),
            OutputFormat::Wat => Err("the WAT format is not yet implemented"),
            _ => Err("this format cannot be used for the current compilation stage"),
        }
    }
}

impl Default for CodegenOutputFormat {
    fn default() -> Self {
        Self::Debug
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OutputKind {
    Lexer(LexerOutputFormat),
    Parser(ParserOutputFormat),
    Typeck(TypeckOutputFormat),
    Codegen(CodegenOutputFormat),
}

pub fn parse_args_or_exit() -> WaccConfig {
    use clap::Command;

    fn command() -> Command {
        command!()
            .arg(
                arg!(files: <FILE> ... "input files")
                    .value_parser(value_parser!(PathBuf))
                    .required(true),
            )
            .arg(
                arg!(-s --stage <STAGE> "the compilation stage to perform")
                    .value_parser(value_parser!(CompilationStage))
                    .required(false),
            )
            .arg(
                arg!(-f --format <FORMAT> "the output format")
                    .value_parser(value_parser!(OutputFormat)),
            )
    }

    fn parse_args() -> Result<WaccConfig, clap::Error> {
        use clap::error::ErrorKind;

        let mut command = command();
        let matches = command.get_matches_mut();

        let paths = matches
            .get_many::<PathBuf>("files")
            .expect("files")
            .cloned()
            .collect();

        let mut stage = matches.get_one::<CompilationStage>("stage").copied();
        let format = matches.get_one::<OutputFormat>("format").copied();

        let output = loop {
            break match (stage, format) {
                (Some(CompilationStage::Lexer), format) => {
                    let format = match format {
                        Some(format) => format.try_into(),
                        None => Ok(Default::default()),
                    };

                    OutputKind::Lexer(
                        format.map_err(|msg| command.error(ErrorKind::ValueValidation, msg))?,
                    )
                }

                (Some(CompilationStage::Parser), format) => {
                    let format = match format {
                        Some(format) => format.try_into(),
                        None => Ok(Default::default()),
                    };

                    OutputKind::Parser(
                        format.map_err(|msg| command.error(ErrorKind::ValueValidation, msg))?,
                    )
                }

                (Some(CompilationStage::Typeck), format) => {
                    let format = match format {
                        Some(format) => format.try_into(),
                        None => Ok(Default::default()),
                    };

                    OutputKind::Typeck(
                        format.map_err(|msg| command.error(ErrorKind::ValueValidation, msg))?,
                    )
                }

                (Some(CompilationStage::Codegen), format) => {
                    let format = match format {
                        Some(format) => format.try_into(),
                        None => Ok(Default::default()),
                    };

                    OutputKind::Codegen(
                        format.map_err(|msg| command.error(ErrorKind::ValueValidation, msg))?,
                    )
                }

                (None, Some(OutputFormat::Coolc)) => OutputKind::Typeck(TypeckOutputFormat::Coolc),
                (None, Some(OutputFormat::Wasm | OutputFormat::Wat)) => {
                    stage = Some(CompilationStage::Codegen);
                    continue;
                }

                (None, _) => {
                    stage = Some(Default::default());
                    continue;
                }
            };
        };

        Ok(WaccConfig { paths, output })
    }

    match parse_args() {
        Ok(cfg) => cfg,

        Err(e) => {
            e.format(&mut command()).exit();
        }
    }
}
