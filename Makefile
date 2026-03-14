EXE = $(shell cabal list-bin gauntlet-exe)

.PHONY: build test clean format repl benchmark benchmark-single help

build:
	cabal build --enable-optimization=2 gauntlet-exe

test:
	cabal test --test-show-details=direct

clean:
	cabal clean

format:
	fourmolu -i src/ test/

repl:
	cabal repl

benchmark: build
	$(EXE) benchmark --config config.json

benchmark-single: build
	$(EXE) benchmark-single --config config.json

help:
	@echo "Available targets: build, test, clean, format, repl, benchmark, benchmark-single"
