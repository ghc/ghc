TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP = ../Setup -v0

# This test is for package reexports
#   1. install p
#   2. install q (reexporting p modules)
#   3. install r (reexporting p and q modules)
#   4. configure and build s, using modules from q and r
#
# Here are the permutations we test for:
#   - Package qualifier? (YES/NO)
#   - Where is module? (defined in SELF /
#         (ORIGinally defined/REEXported) in DEPendency)
#         For deps, could be BOTH, if there is NO package qualifier
#   - Renamed? (YES/NO)
#   - Multiple modules with same name? (YES/NO)
#
# It's illegal for the module to be defined in SELF without renaming, or
# for a package to cause a conflict with itself.  A reexport which does
# not rename definitionally "conflicts" with the original package's definition.
#
# Probably the trickiest bits are when we automatically pick out which package
# when the package qualifier is missing, and handling whether or not modules
# should be exposed or hidden.

cabal05: clean
	$(MAKE) clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	# build p
	cd p && $(SETUP) clean
	cd p && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/$$pkgid'
	cd p && $(SETUP) build
	cd p && $(SETUP) copy
	cd p && $(SETUP) register
	# build q
	cd q && $(SETUP) clean
	cd q && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/$$pkgid'
	cd q && $(SETUP) build
	cd q && $(SETUP) copy
	cd q && $(SETUP) register
	# build r
	cd r && $(SETUP) clean
	cd r && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d --prefix='$(PWD)/$$pkgid'
	cd r && $(SETUP) build
	cd r && $(SETUP) copy
	cd r && $(SETUP) register
	# build s
	cd s && $(SETUP) clean
	cd s && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d
	cd s && $(SETUP) build
	# now test that package recaching works
	rm tmp.d/package.cache
	'$(GHC_PKG)' --no-user-package-db --global-package-db=tmp.d recache
	cd s && $(SETUP) clean
	cd s && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d
	cd s && $(SETUP) build
ifneq "$(CLEANUP)" ""
	$(MAKE) clean
endif

clean :
	'$(GHC_PKG)' unregister --force p >/dev/null 2>&1 || true
	'$(GHC_PKG)' unregister --force q >/dev/null 2>&1 || true
	'$(GHC_PKG)' unregister --force r >/dev/null 2>&1 || true
	$(RM) -r p-* q-* r-* tmp.d *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)
