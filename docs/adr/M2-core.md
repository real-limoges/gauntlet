# ADR M2 — `gauntlet-core`: a Rust-native config vocabulary

Status: **accepted** · Milestone: M2 · See `docs/RUST_PORT.md`. Continues the
clean break from Haskell (`M1-no-oracle.md`): the Haskell tree is untouched and
this is a *redesign*, not a transcription.

## Context

M2 builds `gauntlet-core` — the config/types/error vocabulary plus config loading
(env interpolation, validation, endpoint expansion) and the M1-deferred extraction
that feeds `gauntlet_stats::calculate_stats`. The Haskell types are the *reference*,
not the *contract*: since this is an internal tool with no external consumer of the
old format, the JSON contract is redesigned to be idiomatic Rust rather than ported
verbatim.

## Decisions

1. **snake_case, no renaming.** The JSON config is snake_case and Rust field names
   map 1:1 with zero `#[serde(rename…)]`. Every Haskell field-name prefix
   (`retryMaxAttempts`, `warmupIterations`, `tempoUrl`, `baselineName`, …) is dropped
   — those existed only because Haskell records share one namespace; Rust namespaces
   by type. So `RetrySettings { max_attempts, initial_delay_ms, backoff_multiplier }`,
   `TempoSettings { url, service_name, … }`, etc. This rewrites `examples/*.json`.

2. **Invalid states unrepresentable at the boundary.** `iterations`/`concurrency`/
   delays/timeouts are `NonZeroU32`; `method` is an `HttpMethod` enum (`"GET"` …);
   `#[serde(deny_unknown_fields)]` rejects typos; absent optional sections get real
   defaults (`load_mode` → `Unthrottled`, `log_level` → `Info`, `retry`/`warmup`
   defaulted) instead of `Option`. A zero count or bad method therefore fails at
   *parse* time and never reaches `validate`.

3. **`validate` accumulates.** `BenchmarkConfig::validate` returns `ConfigErrors`
   (a `Vec<String>` with a multi-line `Display`) carrying **every** problem at once
   — better than Haskell's fail-on-first. It only checks what the types can't express
   cheaply: float ranges (`backoff_multiplier ≥ 1.0`, load-mode rates `> 0`),
   non-empty collections (targets, payloads, step lists), and non-empty hook/url
   strings. Messages are field-pathed (`settings.load_mode.steps[0].rpm …`).

4. **schemars-derived schema.** The schema is derived from the types via
   `schemars` (`schema_for!(BenchmarkConfig)`), not embedded. It can never drift; doc
   comments become schema `description`s. `schema/config-schema.json` is regenerated
   from the types (`cargo run -p gauntlet-core --example print_schema`), and a test
   asserts the committed file equals the freshly derived output. This **replaces** the
   old custom `version`/`root`/`types` form-descriptor format, which had no in-repo
   consumer (the `schema` command just re-emitted the embedded file).

5. **`FieldAssertion` is a plain serde enum.** Externally tagged: unit checks
   serialize as bare strings (`"present"`, `"not_null"`), data checks as single-key
   objects (`{"eq": …}`). This deletes the hand-rolled `asum`-aping (de)serializer.

6. **Crate-level `Error` + `Result<T>`.** Only the variants M2 constructs
   (`ReadConfig`, `ParseConfig`, `UndefinedEnvVar`, `Invalid`); the engine/reporters
   extend the surface as they land. No byte-identical Haskell message constraint.

7. **No speculative vocabulary.** `Baseline`/regression types and the runtime
   validation *results* (`ValidationError`/`ValidationSummary`) are **not** defined
   here — they land with their logic in M4 (baselines) and M3 (the engine).
   Consequently `gauntlet-core` does **not** depend on `gauntlet-stats` (the
   extraction test uses it as a dev-dependency), and the `serde` feature briefly added
   to `gauntlet-stats` is reverted — that crate stays `libm`-only.

## Status

Done. 20 `gauntlet-core` tests (examples round-trip + schema-in-sync, parse-time
type rejections, accumulating validation incl. a multi-error case, `LoadMode` and
`FieldAssertion` shapes, `${VAR}` interpolation, extraction → `calculate_stats`)
plus the 25 M1 stats tests — 45 total green; `cargo fmt --check` and
`cargo clippy -D warnings` clean; Haskell tree untouched.
