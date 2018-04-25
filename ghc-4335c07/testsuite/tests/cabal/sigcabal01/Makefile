TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0

# This test is for a Cabal package with holes

sigcabal01:
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' field containers id | sed 's/^.*: *//' > containers
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	cd p && $(SETUP) clean
	cd p && ! $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/inst-p' --ghc-pkg-options="--enable-multi-instance"
	cd p && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/inst-p' --instantiate-with="Map=Data.Map.Lazy@`cat ../containers`" --ghc-pkg-options="--enable-multi-instance"
	cd p && $(SETUP) build
	cd p && $(SETUP) copy
	cd p && $(SETUP) register --print-ipid > ../p_lazy
	cd p && $(SETUP) clean
	cd p && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/inst-p' --instantiate-with="Map=Data.Map.Strict@`cat ../containers`" --ghc-pkg-options="--enable-multi-instance"
	cd p && $(SETUP) build
	cd p && $(SETUP) copy
	cd p && $(SETUP) register --print-ipid > ../p_strict
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db=tmp.d -hide-all-packages -package base -package containers -package-id "`cat p_lazy` (P as P.Lazy)" -package-id "`cat p_strict` (P as P.Strict)" --make Main.hs
	! ./Main
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	'$(GHC_PKG)' unregister --force p >/dev/null 2>&1 || true
	$(RM) -r tmp.d inst-* *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)
