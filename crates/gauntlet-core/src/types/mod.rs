//! The shared type vocabulary: config, units, and response/validation.
//!
//! Validation *results* live here rather than in the engine, so reporters can
//! read them without depending on the measurement loop; the engine owns the
//! checking logic that produces them. Baseline and regression types belong to
//! `gauntlet-report`, next to the store that persists them.

pub mod config;
pub mod response;
pub mod units;
