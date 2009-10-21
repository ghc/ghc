TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# SETUP=../Setup
SETUP=../Setup -v0

# This test is for overlapping/shadowing packages with Cabal.  We
#   1. install p-1.0 to the global DB
#   2. install q-1.0 (depending on p-1.0) to a temporary DB
#   3. install a differnet p-1.0 to the temp DB
#   4. attempt to configure and buidl r, which depends on p-1.0 and q-1.0
#
# step 4 will elicit a warning from Cabal's configure step that the
# packages are inconsistent, and will cause an error from GHC.
# Currently the error message from GHC is swallowed by Cabal.

cabal03: clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	cd p; \
	  $(SETUP) clean; \
	  $(SETUP) configure --with-ghc='$(TEST_HC)'  --disable-optimisation; \
	  $(SETUP) build; $(SETUP) register --inplace;
	cd q; \
	  $(SETUP) configure --with-ghc='$(TEST_HC)' --package-db=../tmp.d; \
	  $(SETUP) build; $(SETUP) register --inplace
	cd p; \
	  $(SETUP) clean; \
	  $(SETUP) configure --with-ghc='$(TEST_HC)' --package-db=../tmp.d; \
	  $(SETUP) build; $(SETUP) register --inplace
	cd r; \
	  ../Setup configure --with-ghc='$(TEST_HC)' --package-db=../tmp.d; \
	  if ../Setup build; then false else true; fi
ifneq "$(CLEANUP)" ""
	$(MAKE) clean
endif

clean :
	'$(GHC_PKG)' unregister --force p >/dev/null 2>&1 || true
	'$(GHC_PKG)' unregister --force q >/dev/null 2>&1 || true
	$(RM) -r tmp.d *.{o,hi} */*.{o,hi} */Setup$(exeext) */dist
