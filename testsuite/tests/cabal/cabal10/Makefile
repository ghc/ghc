TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP = ./Setup -v0

# This test is for packages in internal libraries

cabal10: clean
	$(MAKE) clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	$(SETUP) clean
	$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)'
	$(SETUP) build
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db dist/package.conf.inplace Use.hs
ifneq "$(CLEANUP)" ""
	$(MAKE) clean
endif

clean :
	$(RM) -r */dist Setup$(exeext) *.o *.hi
