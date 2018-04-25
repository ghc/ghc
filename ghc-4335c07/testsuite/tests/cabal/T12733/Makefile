TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0

T12733: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	cp p/P.hs.in1 p/P.hs
	cd p && $(SETUP) clean
	cd p && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d
	cd p && $(SETUP) build
	cd p && $(SETUP) register --inplace
	cd q && $(SETUP) clean
	cd q && $(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=../tmp.d
	cd q && $(SETUP) build
	cd q && $(SETUP) register --inplace
	# Building q should have taken enough time
	cp p/P.hs.in2 p/P.hs
	cd p && $(SETUP) build
	# should fail due to overlapping instance
	cd q && ! $(SETUP) build

ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -r tmp*.d inst-* *.o *.hi */*.o */*.hi */Setup$(exeext) */dist Setup$(exeext)
