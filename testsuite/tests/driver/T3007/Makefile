TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(TEST_WRAPPER)' ./Setup -v0

clean:
	rm -f A/Setup A/Setup.o A/Setup.hi
	rm -f B/Setup B/Setup.o B/Setup.hi
	rm -rf A/dist B/dist
	rm -rf package.conf

# --no-user-package-db to avoid warning about missing settings file

T3007:
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init package.conf
	cd A && '$(TEST_HC)' $(TEST_HC_OPTS) --make Setup -v0
	cd A && $(SETUP) configure --with-compiler='$(TEST_HC)' --ghc-pkg-option=--global-package-db=../package.conf --ghc-pkg-option=--no-user-package-db --ghc-option=-package-db../package.conf
	cd A && $(SETUP) build
	cd A && $(SETUP) register --inplace
	cd B && '$(TEST_HC)' $(TEST_HC_OPTS) --make Setup -v0
	cd B && $(SETUP) configure --with-compiler='$(TEST_HC)' --ghc-pkg-option=--global-package-db=../package.conf --ghc-pkg-option=--no-user-package-db --ghc-option=-package-db../package.conf
	cd B && $(SETUP) build
