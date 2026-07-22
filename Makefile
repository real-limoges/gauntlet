BIN = ./target/release/gauntlet

.PHONY: build test clean format lint check run help

build:
	cargo build --release

test:
	cargo test --workspace

# Format, lint, and test — what CI runs, and what scripts/pre-push enforces.
check: format-check lint test

format:
	cargo fmt --all

format-check:
	cargo fmt --all --check

lint:
	cargo clippy --workspace --all-targets -- -D warnings

clean:
	cargo clean

run: build
	$(BIN) benchmark --config config.json

help:
	@echo "build         cargo build --release"
	@echo "test          cargo test --workspace"
	@echo "check         format-check + lint + test (what CI runs)"
	@echo "format        cargo fmt --all"
	@echo "lint          cargo clippy, warnings denied"
	@echo "clean         cargo clean"
	@echo "run           benchmark against ./config.json"
