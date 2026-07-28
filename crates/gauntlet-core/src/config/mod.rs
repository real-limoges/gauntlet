//! Config loading: `.env` layering + `${VAR}` interpolation, then parse,
//! validate, and expand into endpoints.

pub mod env;
pub mod loader;
