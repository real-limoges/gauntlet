//! Every shipped example config must load and validate.
//!
//! These files are the documented starting points in `examples/` and are what
//! users copy first. Nothing had parsed them with the Rust loader before this,
//! so a field renamed during the port would have surfaced as a user's config
//! error rather than a failing test.

use std::path::PathBuf;

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples")
}

fn example_configs() -> Vec<PathBuf> {
    let mut paths: Vec<PathBuf> = std::fs::read_dir(examples_dir())
        .expect("examples/ exists")
        .flatten()
        .map(|e| e.path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("json"))
        .collect();
    paths.sort();
    assert!(!paths.is_empty(), "there are example configs to check");
    paths
}

#[test]
fn every_example_config_parses_and_validates() {
    let mut failures = Vec::new();

    for path in example_configs() {
        match gauntlet_core::load_benchmark_config(&path) {
            Ok(config) => {
                if let Err(e) = config.validate() {
                    failures.push(format!("{}: {e}", path.display()));
                }
            }
            Err(e) => failures.push(format!("{}: {e}", path.display())),
        }
    }

    assert!(
        failures.is_empty(),
        "example configs failed to load:\n  {}",
        failures.join("\n  ")
    );
}

#[test]
fn every_example_config_expands_to_at_least_one_endpoint() {
    for path in example_configs() {
        let config = gauntlet_core::load_benchmark_config(&path)
            .unwrap_or_else(|e| panic!("{} loads: {e}", path.display()));

        for target in &config.targets {
            let endpoints = gauntlet_core::build_endpoints(&target.url, &config.payloads);
            assert!(
                !endpoints.is_empty(),
                "{} target {} expands to no endpoints",
                path.display(),
                target.name
            );
        }
    }
}
