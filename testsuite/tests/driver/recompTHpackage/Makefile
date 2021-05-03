TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst' --enable-shared

recompTHpackage:
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	# build p
	cp p/Lib1.hs p/Lib.hs
	rm -rf p/dist
	(cd p; $(CONFIGURE) --ipid "p-0.1")
	(cd p; $(SETUP) build)
	(cd p; $(SETUP) copy)
	(cd p; $(SETUP) register)
	# build q
	rm -rf q/dist
	(cd q; $(CONFIGURE) --ipid "q-0.1")
	(cd q; $(SETUP) build)
	(cd q; $(SETUP) copy)
	(cd q; $(SETUP) register)
	./inst/bin/q
	cp p/Lib2.hs p/Lib.hs
	(cd p; $(SETUP) build)
	(cd p; $(SETUP) copy)
	(cd p; $(SETUP) register)
	(cd q; $(SETUP) build)
	(cd q; $(SETUP) copy)
	(cd q; $(SETUP) register)
	./inst/bin/q
