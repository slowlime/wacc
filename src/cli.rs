use std::path::PathBuf;

use clap::{Args, Parser as ClapParser, Subcommand, ValueEnum};
use color_eyre::eyre::Report;

#[derive(ClapParser, Debug, Clone)]
#[command(version)]
pub struct WaccCli {
    #[command(subcommand)]
    pub command: WaccCommand,
}

#[derive(Subcommand, Debug, Clone)]
pub enum WaccCommand {
    Test(TestArgs),
}

#[derive(Args, Debug, Clone)]
pub struct TestArgs {
    /// Input files
    pub paths: Vec<PathBuf>,

    #[arg(short, long)]
    pub stage: TestStage,
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TestStage {
    Lexer,
    Parser,
}

impl WaccCli {
    pub fn run(self) -> Result<(), Report> {
        match self.command {
            WaccCommand::Test(test_args) => TestRunner::new(test_args).run(),
        }
    }
}

struct TestRunner {
    paths: Vec<PathBuf>,
    stage: TestStage,
}

impl TestRunner {
    pub fn new(TestArgs { paths, stage }: TestArgs) -> Self {
        Self { paths, stage }
    }

    pub fn run(self) -> Result<(), Report> {
        match self.stage {
            TestStage::Lexer => self.test_lexer(),
            TestStage::Parser => self.test_parser(),
        }
    }

    fn test_lexer(self) -> Result<(), Report> {
        todo!()
    }

    fn test_parser(self) -> Result<(), Report> {
        todo!()
    }
}
