TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0

# in both cases, p2 should be preferred
cabal08: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp1.d
	'$(GHC_PKG)' init tmp2.d
	'$(TEST_HC)' -v0 --make Setup
	cd p1 && $(SETUP) clean
	cd p1 && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp1.d --prefix='$(PWD)/inst-p1' --ipid="p-0.1-aaa"
	cd p1 && $(SETUP) build
	cd p1 && $(SETUP) copy
	cd p1 && $(SETUP) register
	cd p2 && $(SETUP) clean
	cd p2 && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp2.d --prefix='$(PWD)/inst-p2' --ipid="p-0.1-bbb"
	cd p2 && $(SETUP) build
	cd p2 && $(SETUP) copy
	cd p2 && $(SETUP) register
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db tmp1.d -package-db tmp2.d Main.hs
	./Main
	'$(TEST_HC)' $(TEST_HC_OPTS) -fforce-recomp -package-db tmp2.d -package-db tmp1.d Main.hs
	./Main
	'$(TEST_HC)' $(TEST_HC_OPTS) -fforce-recomp -package-db tmp1.d -package-db tmp2.d -hide-all-packages -package base -package p Main.hs
	./Main
	'$(TEST_HC)' $(TEST_HC_OPTS) -fforce-recomp -package-db tmp2.d -package-db tmp1.d -hide-all-packages -package base -package p Main.hs
	./Main
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -r tmp*.d inst-* *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)
