TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=./Setup -v0

# This tests that we can build a Cabal package that uses TH with both
# profiling and dynamic linking. (#3604)

# omitting -rtsopts from --ghc-options avoids a warning from GHC when
# building the shared library (-rtsopts has no effect with -shared)

cabal04:
	$(MAKE) -s --no-print-directory clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	$(SETUP) clean
	$(SETUP) configure --with-ghc='$(TEST_HC)' --with-hc-pkg='$(GHC_PKG)' --ghc-options='$(filter-out -with-rtsopts="--io-manager=native",$(filter-out -rtsopts,$(TEST_HC_OPTS)))' $(VANILLA) $(PROF) $(DYN)
	$(SETUP) build 2> err
	! grep -v "Creating library file" err
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -r *.o *.hi dist Setup$(exeext) err
