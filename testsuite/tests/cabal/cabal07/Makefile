TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0

# This test is for a Cabal package with thinning and renaming

cabal07: clean
	$(MAKE) clean
	'$(TEST_HC)' -v0 --make Setup
	cd p && $(SETUP) clean
	cd p && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --prefix='$(PWD)/inst-p' --constraint="containers>=0"
	cd p && ! $(SETUP) build
ifneq "$(CLEANUP)" ""
	$(MAKE) clean
endif

clean :
	'$(GHC_PKG)' unregister --force p >/dev/null 2>&1 || true
	$(RM) -r tmp.d inst-* *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)
