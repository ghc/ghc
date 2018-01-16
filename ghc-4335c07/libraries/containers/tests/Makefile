# The tests should be compiled and run using cabal:
# > cabal configure --enable-tests
# > cabal build
# > cabal test
#
# This Makefile is used by developers to compile the tests manually.

all:

%-properties: %-properties.hs force
	ghc -I../include -O2 -DTESTING $< -i.. -o $@ -outputdir tmp

%-strict-properties: %-properties.hs force
	ghc -I../include -O2 -DTESTING -DSTRICT $< -o $@ -i.. -outputdir tmp

.PHONY: force clean
force:

clean:
	rm -rf tmp $(patsubst %.hs, %, $(wildcard *-properties.hs)) $(patsubst %-properties.hs, %-strict-properties, $(wildcard *-properties.hs))
