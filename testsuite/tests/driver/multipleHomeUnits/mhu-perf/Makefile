TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0

mhu-perf: clean
	$(MAKE) -s --no-print-directory clean
	./genLargeHMU
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	for dir in unit-p*; do \
	  cd $$dir && $(SETUP) clean && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --ipid=$$dir-0.1.0.0 --with-ghc='$(TEST_HC)' --with-hc-pkg='$(GHC_PKG)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d && $(SETUP) build && $(SETUP) register --inplace && cd ..; \
  done;


ifeq "$(CLEANUP)" "1"
	$(MAKE) -s --no-print-directory clean
endif

clean :
	 $(RM) -r unitTop* unit-p* top*/ tmp*.d inst-* *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)

