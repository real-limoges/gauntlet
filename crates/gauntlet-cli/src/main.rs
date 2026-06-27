//! `gauntlet` binary — CLI parsing (clap) and dispatch to the four subcommands
//! (benchmark / compare / validate / schema).
//!
//! Wired up in milestone **M6**. See `docs/RUST_PORT.md`.
//! Maps from the Haskell tree: `app/Main.hs`, `Lib.hs`, `Benchmark/Config/CLI.hs`.

fn main() {
    // Exit-code contract (preserved from the Haskell `RunResult`):
    //   0 = success, 1 = regression detected, 2 = error.
    eprintln!("gauntlet (Rust port) — not yet implemented; see docs/RUST_PORT.md");
    std::process::exit(2);
}
