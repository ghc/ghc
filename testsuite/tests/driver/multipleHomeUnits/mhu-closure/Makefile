TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0

mhu-closure: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	cd p && $(SETUP) clean
	cd p && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --ipid=p-0.1.0.0 --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d
	cd p && $(SETUP) build
	cd p && $(SETUP) register --inplace
	cd q && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --ipid=q-0.1.0.0 --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d
	cd q && $(SETUP) build
	cd q && $(SETUP) register --inplace
	cd r && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --ipid=r-0.1.0.0 --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d
	cd r && $(SETUP) build
	cd r && $(SETUP) register --inplace
	# This should work
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp -unit @unitP
	# So should this
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp -unit @unitP -unit @unitQ
	# So should this
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp -unit @unitP -unit @unitQ -unit @unitR
	# This should error with a closure message
	! '$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp -unit @unitP -unit @unitR
	# This should work, even though r1 is not in the package db
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp -unit @unitP -unit @unitQ -unit @unitR1
	# This should fail, even though r1 is not in the package db
	! '$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp -unit @unitP -unit @unitR1


ifeq "$(CLEANUP)" "1"
	$(MAKE) -s --no-print-directory clean
endif

clean :
	 $(RM) -r tmp*.d inst-* *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)

