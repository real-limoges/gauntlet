//! `gauntlet-tracing` — Grafana Tempo distributed-trace querying and analysis
//! for the benchmark time window.
//!
//! NOTE: this is *Tempo* tracing, not the `tracing` logging crate; logging is the
//! small hand-rolled `gauntlet_core::log`.
//!
//! # What it does
//!
//! After a benchmark finishes, the run has a `[start, end]` window. This crate
//! asks Tempo which traces for the configured service fall inside it, fetches
//! them, and reduces them to per-(service, span) latency statistics that the
//! reporters render next to the HTTP-level numbers. It answers "the P99 got
//! worse — *where*?", which client-side measurement alone cannot.
//!
//! # The one rule
//!
//! Trace analysis is a **diagnostic and must never fail the run**. Tempo is a
//! separate service that can be down, slow, or simply not have ingested the run's
//! spans yet. Accordingly:
//!
//! * [`analyze`] returns `Ok(None)` for exactly one reason — tracing is disabled
//!   in config. A window that legitimately contains no traces yields an *empty*
//!   [`TraceAnalysis`], so the caller can tell "not configured" from "configured,
//!   but Tempo had nothing", which are very different things to be told at 2am.
//! * Every other problem is an [`Error`] for the caller to **log and move on
//!   from**. Nothing here should influence the process exit code, which is why
//!   the error messages are written for a human debugging their Tempo setup and
//!   always name the URL.
//!
//! # Grouping, and the wire format
//!
//! Spans are grouped by **(service, span name)**, not by name alone. The moment a
//! trace crosses services, `handle` in the gateway and `handle` in the backend
//! would otherwise collapse into one row with a bimodal, meaningless
//! distribution. Results sort by P95 descending, with the key as a tiebreaker so
//! equal-latency rows do not reshuffle between runs.
//!
//! [`types`] is deliberately *not* the wire format. Tempo's JSON is OTLP shaped —
//! nested `batches → scopeSpans → spans`, timestamps as decimal strings because
//! they exceed JSON's safe integer range, attributes as `{key, value:
//! {stringValue: …}}` triples. Decoding lives in [`client`]; everything
//! downstream works with the flat, already-typed [`Span`].
//!
//! That decoding is **deliberately forgiving**. Producers disagree about whether
//! timestamps are strings or numbers, and whether span kind and status are enum
//! names or ordinals. Anything unrecognized degrades to a default rather than
//! failing: a trace backend is not a contract this tool controls, and a
//! diagnostic that refuses to render because one span had an unfamiliar status is
//! worse than useless.
//!
//! Only two endpoints are used: `GET /api/search` for lightweight metadata, and
//! `GET /api/traces/<id>` for the full trace. Search returns no spans, so a
//! window costs one search plus one fetch per trace, and the fetches run with
//! bounded concurrency — a busy window produces hundreds, and serializing them
//! would let trace analysis dominate the run's wall clock.
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
