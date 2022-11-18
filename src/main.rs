pub mod analysis;
pub mod ast;
pub mod errors;
pub mod parse;
pub mod position;
pub mod source;
mod ui;
pub mod util;

use clap::Parser as ClapParser;
use color_eyre::eyre::Report;
use tracing_subscriber::filter::{EnvFilter, LevelFilter};
use tracing_subscriber::prelude::*;

const LOG_ENV_NAME: &str = "WACC_LOG";

pub fn main() -> Result<(), Report> {
    let filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .with_env_var(LOG_ENV_NAME)
        .from_env_lossy();
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(filter)
        .init();

    ui::WaccCli::parse().run()
}
