EXE = $(shell cabal list-bin perf-testing-exe)

build:
	cabal build --enable-optimization=2 perf-testing-exe

benchmark-multiple: build
	$(EXE) benchmark-multiple --config config.json

benchmark-single: build
	$(EXE) benchmark-single --config config.json

verify: build
	$(EXE) verify --config config.json