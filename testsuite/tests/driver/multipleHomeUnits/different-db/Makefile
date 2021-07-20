TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0

different-db: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(GHC_PKG)' init tmp1.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	# Put p into tmp.d
	cd p && $(SETUP) clean
	cd p && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --ipid=p-0.1.0.0 --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d
	cd p && $(SETUP) build
	cd p && $(SETUP) register --inplace
	# Put p1 into tmp1.d
	cd p1 && $(SETUP) clean
	cd p1 && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --ipid=p1-0.1.0.0 --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp1.d
	cd p1 && $(SETUP) build
	cd p1 && $(SETUP) register --inplace
	# This should work
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp -unit @unitP -unit @unitQ
	# So should this
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp -unit @unitP1 -unit @unitR
	# So should this
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp -unit @unitP -unit @unitQ -unit @unitR -unit @unitP1
	# So should this?
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp -unit @unitQ -unit @unitR


ifeq "$(CLEANUP)" "1"
	$(MAKE) -s --no-print-directory clean
endif

clean :
	 $(RM) -r tmp*.d inst-* *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)

