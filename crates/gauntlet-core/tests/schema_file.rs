//! The checked-in schema must match the one derived from the config types.
//!
//! `schema/config-schema.json` is committed so consumers (editors, CI linters)
//! can reference it without running the binary. Since it is now *derived* by
//! schemars rather than hand-maintained, the only failure mode is drift: a field
//! is added to the config types and the committed copy goes stale. This test is
//! the thing that notices.
//!
//! When it fails, regenerate: `cargo run -p gauntlet-cli --bin gauntlet -- \
//! schema --out schema/config-schema.json`

use std::path::PathBuf;

#[test]
fn the_committed_schema_matches_the_derived_one() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../schema/config-schema.json");
    let committed = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("{} is readable: {e}", path.display()));

    // Compare parsed values, not bytes: trailing-newline and formatting
    // differences are not drift.
    let committed: serde_json::Value =
        serde_json::from_str(&committed).expect("the committed schema is valid JSON");
    let derived: serde_json::Value = serde_json::from_str(&gauntlet_core::config_schema_string())
        .expect("derived is valid JSON");

    assert_eq!(
        committed, derived,
        "schema/config-schema.json is stale — regenerate it with \
         `cargo run -p gauntlet-cli --bin gauntlet -- schema --out schema/config-schema.json`"
    );
}
