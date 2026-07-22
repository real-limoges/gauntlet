//! `gauntlet-tracing` — Grafana Tempo distributed-trace querying and analysis
//! for the benchmark time window.
//!
//! NOTE: this is *Tempo* tracing, not the `tracing` logging crate (see
//! `docs/adr/M0-foundation.md`). Ported in milestone **M6** from the Haskell
//! `Tracing/*.hs` and `Runner/Tracing.hs`.
//!
//! # What it does
//!
//! After a benchmark finishes, the run has a `[start, end]` window. This crate
//! asks Tempo which traces for the configured service fall inside it, fetches
//! them, and reduces them to per-(service, span) latency statistics that the
//! reporters render next to the HTTP-level numbers. It answers "the P99 got
//! worse — *where*?", which the client-side measurement alone cannot.
//!
//! # The one rule
//!
//! Trace analysis is a **diagnostic and must never fail the run** (ADR M6-A
//! §7). Tempo is a separate service that can be down, slow, or simply not have
//! ingested the run's spans yet. Accordingly:
//!
//! * [`analyze`] returns `Ok(None)` for exactly one reason — tracing is
//!   disabled in config. A window with no traces yields an *empty*
//!   [`TraceAnalysis`], which is a different and reportable fact.
//! * Every other problem is an [`Error`] for the caller to **log and move on
//!   from**. Nothing in this crate should influence the process exit code.
//!
//! # Example
//!
//! ```no_run
//! # async fn example(settings: &gauntlet_core::TempoSettings) {
//! use gauntlet_tracing::{analyze, render_terminal, TraceWindow};
//!
//! let window = TraceWindow::from_unix_nanos(start_ns(), end_ns());
//! match analyze(settings, window).await {
//!     Ok(Some(analysis)) => println!("{}", render_terminal(&analysis)),
//!     Ok(None) => {}                               // tracing disabled
//!     Err(e) => eprintln!("trace analysis skipped: {e}"), // never fatal
//! }
//! # }
//! # fn start_ns() -> u64 { 0 }
//! # fn end_ns() -> u64 { 0 }
//! ```

pub mod analysis;
pub mod client;
pub mod error;
pub mod report;
pub mod types;

pub use analysis::{
    aggregate_spans, analyze, build_analysis, is_enabled, SpanAggregation, TraceAnalysis,
};
pub use client::TempoClient;
pub use error::{Error, Result};
pub use report::{render_markdown, render_terminal, write_raw_traces};
pub use types::{
    Span, SpanKind, SpanStatus, Trace, TraceMetadata, TraceQuery, TraceWindow, DEFAULT_SEARCH_LIMIT,
};
