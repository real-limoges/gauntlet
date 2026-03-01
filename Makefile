EXE = $(shell cabal list-bin gauntlet-exe)

build:
	cabal build --enable-optimization=2 gauntlet-exe

benchmark-nway: build
	$(EXE) benchmark-nway --config config.json

benchmark-single: build
	$(EXE) benchmark-single --config config.json

verify: build
	$(EXE) verify --config config.json