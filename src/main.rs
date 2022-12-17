pub mod analysis;
pub mod ast;
pub mod codegen;
pub mod errors;
pub mod parse;
pub mod position;
mod runner;
pub mod source;
pub mod util;

use std::io;
use std::process::ExitCode;

use tracing_subscriber::filter::{EnvFilter, LevelFilter};
use tracing_subscriber::prelude::*;

const LOG_ENV_NAME: &str = "WACC_LOG";

pub fn main() -> ExitCode {
    let filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .with_env_var(LOG_ENV_NAME)
        .from_env_lossy();
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().with_writer(io::stderr))
        .with(filter)
        .init();

    runner::prepare_and_run()
}
