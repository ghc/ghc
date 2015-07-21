TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0

# This test is for two Cabal packages exposing the same signature

sigcabal02:
	$(MAKE) clean
	'$(GHC_PKG)' field containers id | sed 's/^.*: *//' > containers
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	cd p && $(SETUP) clean
	cd p && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/inst-p' --instantiate-with="Map=Data.Map.Lazy@`cat ../containers`" --instantiate-with="Set=Data.Set@`cat ../containers`" --ghc-pkg-options="--enable-multi-instance"
	cd p && $(SETUP) build
	cd p && $(SETUP) copy
	cd p && $(SETUP) register --print-ipid > ../p_ipid
	cd q && $(SETUP) clean
	cd q && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/inst-p' --instantiate-with="Map=Data.Map.Lazy@`cat ../containers`" --ghc-pkg-options="--enable-multi-instance"
	cd q && $(SETUP) build
	cd q && $(SETUP) copy
	cd q && $(SETUP) register --print-ipid > ../q_ipid
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db=tmp.d -hide-all-packages -package base -package-id "`cat p_ipid`" -package-id "`cat q_ipid`" --make Main.hs
	./Main
	! '$(TEST_HC)' $(TEST_HC_OPTS) -package-db=tmp.d -hide-all-packages -package base -package-id "`cat p_ipid`" -package-id "`cat q_ipid`" --make ShouldFail.hs
ifneq "$(CLEANUP)" ""
	$(MAKE) clean
endif

clean :
	'$(GHC_PKG)' unregister --force p >/dev/null 2>&1 || true
	'$(GHC_PKG)' unregister --force q >/dev/null 2>&1 || true
	$(RM) -r tmp.d inst-* *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)
