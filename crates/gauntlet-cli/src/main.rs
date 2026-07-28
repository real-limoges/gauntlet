//! The `gauntlet` binary: parse arguments, dispatch, exit with the semantic code.
//!
//! # Exit codes are the CI contract
//!
//! `0` success, `1` regression detected, `2` error. `RunOutcome` owns that
//! mapping and this file's only job is to call it once — no other module calls
//! `std::process::exit`, and every command returns a `RunOutcome` rather than
//! exiting on its own.
//!
//! One consequence is worth knowing when touching the engine: `process::exit`
//! does not unwind, so no destructor runs. Anything buffered must be flushed
//! explicitly rather than left to `Drop`.
//!
//! # What lives in the binary, and why
//!
//! [`adapter`] converts the engine's measurements into the report model. It sits
//! here rather than in `gauntlet-report` so that crate stays free of an engine
//! dependency, and a change to the measurement loop does not recompile every
//! renderer. Two things happen there that neither the engine nor the stats crate
//! can do alone: a target's endpoints are flattened into one latency population,
//! and Earth Mover's Distance is attached to each pairwise comparison, since EMD
//! needs the raw duration vectors that `compare_bayesian` never sees.
//!
//! [`reporters`] is the whole "add an output format" seam: implement `Reporter`,
//! add a flag, push it there. The terminal backend is unconditional — a run that
//! printed nothing would look like a run that did nothing — and everything else
//! is opt-in, except the CI reporter, which activates on detecting a CI
//! environment.
//!
//! The reporter set is built **once** and reused for both the benchmark and the
//! regression pass. Handing the regression pass a fresh `TerminalReporter`
//! instead leaves every other backend's `on_regression` unreachable, including
//! `CiReporter`, whose only method that is.
//!
//! # Two things the flag surface decides
//!
//! `--charts` parses into `ChartKind` rather than free text, so an unknown kind
//! fails at parse time with the valid list rather than after the benchmark has
//! been paid for. It also selects the kinds for *every* renderer that draws, not
//! just the standalone SVG files: asking for `--charts error_rate` and getting a
//! histogram in the HTML is not what the flag says.
//!
//! The live view activates only for a human at an interactive terminal, keyed off
//! **stdout** being a TTY. Keying off stdin is the classic mistake: `gauntlet
//! benchmark < /dev/null` would disable the UI at a perfectly interactive
//! terminal, and a redirected stdout would fill the transcript with escape
//! sequences.

// Production code must not panic: an unwrap that fires mid-run destroys the
// whole measurement, and a benchmark that dies is worse than one reporting a
// clean error. `cfg(not(test))` scopes this to real code; inside `#[cfg(test)]`
// modules, panicking assertions and exact float comparisons are the point.
#![cfg_attr(
    not(test),
    deny(
        clippy::unwrap_used,
        clippy::panic,
        clippy::unreachable,
        clippy::panic_in_result_fn,
        clippy::float_cmp
    )
)]

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
