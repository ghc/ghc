TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst' --enable-executable-dynamic

dynamicTooMake: clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	# build p
	rm -rf p/dist
	(cd p; $(CONFIGURE) --enable-shared --ipid "p-0.1")
	(cd p; $(SETUP) build)
	(cd p; $(SETUP) copy)
	(cd p; $(SETUP) register)
	(cd p; echo "q = 0" >> Lib.hs)
	# build p, but only rebuild hi
	(cd p; $(CONFIGURE) --disable-shared --ipid "p-0.1")
	(cd p; $(SETUP) build)
	(cd p; $(SETUP) copy)
	(cd p; $(SETUP) register)
	(cd q; $(CONFIGURE) --disable-shared --ipid "q-0.1")
	# build q, should be an error as p has mismatched .hi and .dyn_hi files
	(cd q; $(SETUP) build) || true

clean :
	$(RM) -r tmp.d inst dist Setup$(exeext)
