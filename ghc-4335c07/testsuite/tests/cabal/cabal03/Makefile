TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# SETUP=../Setup
SETUP=../Setup -v0

# This test is for overlapping/shadowing packages with Cabal.  We
#   1. install p-1.0
#   2. install q-1.0 (depending on p-1.0)
#   3. install a different p-1.0
#   4. attempt to configure and buidl r, which depends on p-1.0 and q-1.0
#
# step 4 will elicit a warning from Cabal's configure step that the
# packages are inconsistent, and will cause an error from GHC.
# Currently the error message from GHC is swallowed by Cabal.

cabal03: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	cd p && $(SETUP) clean
	cd p && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --disable-optimisation --ghc-pkg-option=--force --ipid p-noopt
	cd p && $(SETUP) build
	cd p && $(SETUP) register
	cd q && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --ghc-pkg-option=--force
	cd q && $(SETUP) build
	cd q && $(SETUP) register
	cd p && $(SETUP) clean
	cd p && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --ghc-pkg-option=--force --ipid p-withopt
	cd p && $(SETUP) build
	cd p && $(SETUP) register
	cd r && ! ../Setup configure -v0 $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --ghc-pkg-option=--force
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	'$(GHC_PKG)' unregister --force p >/dev/null 2>&1 || true
	'$(GHC_PKG)' unregister --force q >/dev/null 2>&1 || true
	$(RM) -r tmp.d *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)
