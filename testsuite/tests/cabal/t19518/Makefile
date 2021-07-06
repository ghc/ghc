TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP = ../Setup -v0

# This test is for package reexports and -Wunused-packages working together
#   1. install p
#   2. install q (reexporting p modules)
#   3. install r (using reexports from q)
#
# When building r, it should not suggest the import of q package is redundant

t19518: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
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
	cd r && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS) -Wunused-packages' --package-db=../tmp.d --prefix='$(PWD)/$$pkgid'
	cd r && $(SETUP) build
	cd r && $(SETUP) copy
	cd r && $(SETUP) register
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	'$(GHC_PKG)' unregister --force p >/dev/null 2>&1 || true
	'$(GHC_PKG)' unregister --force q >/dev/null 2>&1 || true
	'$(GHC_PKG)' unregister --force r >/dev/null 2>&1 || true
	$(RM) -r p-* q-* r-* s-* t-* tmp.d *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)
