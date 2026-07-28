BIN = ./target/release/gauntlet

CONFIG ?= examples/simple-benchmark.json

.PHONY: build test clean format format-check lint doc check run schema help

build:
	cargo build --release

test:
	cargo test --workspace --all-features --locked

# Format, lint, docs, and test: what CI runs, and what scripts/pre-push
# enforces. Keep this in step with .github/workflows/ci.yml.
check: format-check lint doc test

format:
	cargo fmt --all

format-check:
	cargo fmt --all --check

lint:
	cargo clippy --workspace --all-targets --all-features --locked -- -D warnings

# Broken intra-doc links fail nothing else in the build.
doc:
	RUSTDOCFLAGS="-D warnings" cargo doc --workspace --no-deps --locked

clean:
	cargo clean

# Override with `make run CONFIG=path/to/your.json`. The default is a committed
# example so a fresh clone can run the target without inventing a config first.
run: build
	$(BIN) benchmark --config $(CONFIG)

# Regenerate the committed schema after changing the config types.
schema:
	cargo run -p gauntlet-cli --bin gauntlet -- schema --out schema/config-schema.json

help:
	@echo "build         cargo build --release"
	@echo "test          cargo test, all features, locked"
	@echo "check         format-check + lint + doc + test (what CI runs)"
	@echo "format        cargo fmt --all"
	@echo "lint          cargo clippy, warnings denied"
	@echo "doc           cargo doc, broken intra-doc links denied"
	@echo "clean         cargo clean"
	@echo "run           benchmark against \$$(CONFIG) [$(CONFIG)]"
	@echo "schema        regenerate schema/config-schema.json from the types"
