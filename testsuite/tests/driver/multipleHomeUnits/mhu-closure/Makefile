TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0
CONFIGURE=configure \
	  $(CABAL_MINIMAL_BUILD) \
	  --with-ghc='$(TEST_HC)' \
	  --with-hc-pkg='$(GHC_PKG)' \
	  --ghc-options='$(TEST_HC_OPTS)' \
	  --package-db=../tmp.d
TEST_BUILD='$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -fforce-recomp
ONLY_BASE=-hide-all-packages -package base

mhu-closure: clean pkg-database
	! $(TEST_BUILD) -unit @unitP
	! $(TEST_BUILD) -unit @unitP -unit @unitQ
	! $(TEST_BUILD) -unit @unitP -unit @unitR
	! $(TEST_BUILD) -unit @unitP -unit @unitR1
	$(TEST_BUILD) -unit @unitP -unit @unitQ -unit @unitR
	! $(TEST_BUILD) -unit @unitP -unit @unitQ -unit @unitR1
	$(TEST_BUILD) $(ONLY_BASE) -unit @unitP
	$(TEST_BUILD) $(ONLY_BASE) -unit @unitP -unit @unitQ
	! $(TEST_BUILD) $(ONLY_BASE) -unit @unitP -unit @unitR
	! $(TEST_BUILD) $(ONLY_BASE) -unit @unitP -unit @unitR1
	$(TEST_BUILD) $(ONLY_BASE) -unit @unitP -unit @unitQ -unit @unitR
	$(TEST_BUILD) $(ONLY_BASE) -unit @unitP -unit @unitQ -unit @unitR1

.PHONY: pkg-database
pkg-database:
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	cd p && $(SETUP) $(CONFIGURE) --ipid=p-0.1.0.0
	cd p && $(SETUP) build
	cd p && $(SETUP) register --inplace
	cd q && $(SETUP) $(CONFIGURE) --ipid=q-0.1.0.0
	cd q && $(SETUP) build
	cd q && $(SETUP) register --inplace
	cd r && $(SETUP) $(CONFIGURE) --ipid=r-0.1.0.0
	cd r && $(SETUP) build
	cd r && $(SETUP) register --inplace

.PHONY: clean
clean:
	$(RM) -r tmp.d Setup$(exeext) */dist* *.hi *.o */*.hi */*.o

ifeq "$(CLEANUP)" "1"
	$(MAKE) -s --no-print-directory clean
endif
