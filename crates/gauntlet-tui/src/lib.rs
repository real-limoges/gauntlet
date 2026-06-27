//! `gauntlet-tui` — the live terminal UI (ratatui/crossterm) and the
//! `BenchmarkEvent` state machine driven over an mpsc channel.
//!
//! Populated in milestone **M5**. See `docs/RUST_PORT.md`.
//!
//! Maps from the Haskell tree: `Benchmark/TUI*.hs` (Brick/vty + STM `TBQueue`).
