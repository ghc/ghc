TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0

# This test is for packages whose package IDs overlap, but whose IPIDs
# do not.
#
#   1. install p-1.0
#   2. install q-1.0 (depending on p-1.0)
#   3. install p-1.1
#   4. install q-1.0, asking for p-1.1
#   5. install r-1.0 (depending on p-1.1, q-1.0)
#   6. install r-1.0 asking for p-1.0
#
# The notable steps are (4), which previously would have required a reinstall,
# and (6), where the dependency solver picks between two IPIDs with the
# same package ID based on their depenencies.
#
# ./Setup configure is pretty dumb, so we spoonfeed it precisely the
# dependencies it needs.

cabal06: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	cd p-1.0 && $(SETUP) clean
	cd p-1.0 && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/inst-a' --ghc-pkg-options='--enable-multi-instance'
	cd p-1.0 && $(SETUP) build
	cd p-1.0 && $(SETUP) copy
	(cd p-1.0 && $(SETUP) register --print-ipid) > tmp_p_1_0
	cd q && $(SETUP) clean
	cd q && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/inst-b' --ghc-pkg-options='--enable-multi-instance'
	cd q && $(SETUP) build
	cd q && $(SETUP) copy
	(cd q && $(SETUP) register --print-ipid) > tmp_first_q
	cd p-1.1 && $(SETUP) clean
	cd p-1.1 && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/inst-c' --ghc-pkg-options='--enable-multi-instance'
	cd p-1.1 && $(SETUP) build
	cd p-1.1 && $(SETUP) copy
	(cd p-1.1 && $(SETUP) register --print-ipid) > tmp_p_1_1
	cd q && $(SETUP) clean
	cd q && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --constraint="p==1.1" --prefix='$(PWD)/inst-d' --ghc-pkg-options='--enable-multi-instance'
	cd q && $(SETUP) build
	cd q && $(SETUP) copy
	(cd q && $(SETUP) register --print-ipid) > tmp_second_q
	@echo "Does the first instance of q depend on p-1.0?"
	'$(GHC_PKG)' field --ipid `cat tmp_first_q` depends -f tmp.d | grep `cat tmp_p_1_0` | wc -l | sed 's/[[:space:]]//g'
	@echo "Does the second instance of q depend on p-1.0?"
	'$(GHC_PKG)' field --ipid `cat tmp_second_q` depends -f tmp.d | grep `cat tmp_p_1_1` | wc -l | sed 's/[[:space:]]//g'
	cd r && $(SETUP) clean
	cd r && ../Setup configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --dependency="q=`cat ../tmp_first_q`" --constraint="p==1.0" --prefix='$(PWD)/inst-e' --ghc-pkg-options='--enable-multi-instance'
	cd r && $(SETUP) build
	cd r && $(SETUP) copy
	cd r && $(SETUP) clean
	cd r && ../Setup configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --dependency="q=`cat ../tmp_second_q`" --constraint="p==1.1" --prefix='$(PWD)/inst-f' --ghc-pkg-options='--enable-multi-instance'
	cd r && $(SETUP) build
	cd r && $(SETUP) copy
	inst-e/bin/cabal06
	inst-f/bin/cabal06
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	'$(GHC_PKG)' unregister --force p >/dev/null 2>&1 || true
	'$(GHC_PKG)' unregister --force q >/dev/null 2>&1 || true
	'$(GHC_PKG)' unregister --force r >/dev/null 2>&1 || true
	$(RM) -r tmp.d inst-* *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)
