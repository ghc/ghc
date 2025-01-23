CABAL := $(shell cabal list-bin -v0 --project-dir libraries/Cabal cabal-install:exe:cabal)

all: $(CABAL)
	CABAL=$(CABAL) ./Build.hs

$(CABAL):
	cabal build --project-dir libraries/Cabal cabal

clean:
	rm -rf _build

