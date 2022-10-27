pub mod ast;
mod cli;
pub mod cursor;
pub mod lexer;
pub mod parser;
pub mod position;
pub mod source;
pub mod token;
pub mod util;

use clap::Parser as ClapParser;
use color_eyre::eyre::Report;

pub fn main() -> Result<(), Report> {
    cli::WaccCli::parse().run()
}
