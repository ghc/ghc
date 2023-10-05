TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

warnings-ghc-deferred: clean
	"$GHC" --make -Wall -fdefer-diagnostics src/Lib.hs src/Lib/A.hs src/Lib/B.hs"
	ghc --version

warnings-ghc-regular: clean
	bash -c "ghc --make -Wall src/Lib.hs src/Lib/A.hs src/Lib/B.hs"
	ghc --version

.PHONY: warnings-ghc

clean:
	rm -rf src/**/*.{hi,o}
	rm -rf **/*.{hi,o}

.PHONY: clean
