EXE = $(shell cabal list-bin laughing-waffle-exe)

build:
	cabal build --enable-optimization=2 laughing-waffle-exe

benchmark-multiple: build
	$(EXE) benchmark-multiple --config config.json

benchmark-single: build
	$(EXE) benchmark-single --config config.json

verify: build
	$(EXE) verify --config config.json