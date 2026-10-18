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

# Each case announces itself and merges stderr into stdout, so that
# mhu-closure.check can pin the expected outcome per case (CHECK-LABEL).
mhu-closure: clean pkg-database
	@echo "== P =="
	! $(TEST_BUILD) -unit @unitP 2>&1
	@echo "== P Q =="
	! $(TEST_BUILD) -unit @unitP -unit @unitQ 2>&1
	@echo "== P R =="
	! $(TEST_BUILD) -unit @unitP -unit @unitR 2>&1
	@echo "== P R1 =="
	! $(TEST_BUILD) -unit @unitP -unit @unitR1 2>&1
	@echo "== P Q R =="
	$(TEST_BUILD) -unit @unitP -unit @unitQ -unit @unitR 2>&1
	@echo "== P Q R1 =="
	! $(TEST_BUILD) -unit @unitP -unit @unitQ -unit @unitR1 2>&1
	@echo "== base-only P =="
	$(TEST_BUILD) $(ONLY_BASE) -unit @unitP 2>&1
	@echo "== base-only P Q =="
	$(TEST_BUILD) $(ONLY_BASE) -unit @unitP -unit @unitQ 2>&1
	@echo "== base-only P R =="
	! $(TEST_BUILD) $(ONLY_BASE) -unit @unitP -unit @unitR 2>&1
	@echo "== base-only P R1 =="
	! $(TEST_BUILD) $(ONLY_BASE) -unit @unitP -unit @unitR1 2>&1
	@echo "== base-only P Q R =="
	$(TEST_BUILD) $(ONLY_BASE) -unit @unitP -unit @unitQ -unit @unitR 2>&1
	@echo "== base-only P Q R1 =="
	$(TEST_BUILD) $(ONLY_BASE) -unit @unitP -unit @unitQ -unit @unitR1 2>&1

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
