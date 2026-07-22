//! The `gauntlet` binary: parse arguments, dispatch, exit with the semantic code.
//!
//! Exit codes are the CI contract: `0` success, `1` regression detected, `2`
//! error. `RunOutcome` owns that mapping (ADR M4-report §6); this file's only
//! job is to call it once.
//!
//! The Haskell `app/Main.hs` called C `exit()` directly to bypass GHC's slow
//! finalizer and connection teardown. That has no Rust equivalent and is gone —
//! tokio's shutdown is not on the critical path.

mod adapter;
mod cli;
mod commands;
mod reporters;

use clap::Parser;
use gauntlet_report::RunOutcome;

use cli::{Cli, Command};

#[tokio::main]
async fn main() {
    let outcome = dispatch(Cli::parse().command).await.unwrap_or_else(|e| {
        // `{e:#}` prints the whole anyhow context chain, so the message says
        // which file or target failed rather than just the leaf cause.
        eprintln!("error: {e:#}");
        RunOutcome::Error
    });

    std::process::exit(outcome.exit_code());
}

async fn dispatch(command: Command) -> anyhow::Result<RunOutcome> {
    match command {
        Command::Benchmark(args) => commands::benchmark(&args).await,
        Command::Compare { file_a, file_b } => commands::compare(&file_a, &file_b).await,
        Command::Validate {
            config,
            check_endpoints,
        } => commands::validate(&config, check_endpoints).await,
        Command::Schema { out } => commands::schema(out.as_ref()),
    }
}
