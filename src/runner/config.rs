use std::path::PathBuf;

use clap::{Args, Parser as ClapParser, ValueEnum, CommandFactory, FromArgMatches};

#[derive(ClapParser, Debug, Clone)]
#[command(version)]
pub struct WaccConfig {
    /// The input files
    pub paths: Vec<PathBuf>,

    #[command(flatten)]
    pub output: OutputArgs,
}

/// The output to generate
#[derive(Args, Debug, Clone)]
pub struct OutputArgs {
    /// The compilation stage to stop and dump the output at
    #[arg(short, long, value_enum, default_value_t)]
    pub stage: CompilationStage,

    /// The format of the output
    #[arg(short, long, value_enum)]
    pub format: Option<OutputFormat>,
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CompilationStage {
    Lexer,
    Parser,
    Typeck,
}

impl Default for CompilationStage {
    fn default() -> Self {
        Self::Typeck
    }
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OutputFormat {
    Coolc,
    Debug,
}

pub fn parse_args_or_exit() -> WaccConfig {
    fn parse_args() -> Result<WaccConfig, clap::Error> {
        let command = WaccConfig::command();
        let mut matches = command.try_get_matches()?;
        let cfg = WaccConfig::from_arg_matches_mut(&mut matches)?;

        Ok(cfg)
    }

    match parse_args() {
        Ok(cfg) => cfg,

        Err(e) => {
            e.format(&mut WaccConfig::command()).exit();
        }
    }
}
