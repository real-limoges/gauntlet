//! The shared type vocabulary: config, units, and response/validation.
//!
//! Baseline/regression types land with their save-load logic in M4; runtime
//! validation *results* land with the engine in M3.

pub mod config;
pub mod response;
pub mod units;
