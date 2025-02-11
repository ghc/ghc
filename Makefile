export CABAL := $(shell cabal list-bin -v0 --project-dir libraries/Cabal cabal-install:exe:cabal)

all: $(CABAL)
	GHC=ghc-9.8.4 ./Build.hs

cabal: $(CABAL)
	
$(CABAL):
	cabal build --project-dir libraries/Cabal cabal-install:exe:cabal

clean:
	rm -rf _build

test: all
	TEST_HC=`pwd`/_build/bindist/bin/ghc make -C testsuite/tests test
