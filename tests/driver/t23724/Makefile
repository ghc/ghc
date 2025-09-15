TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(filter-out -rtsopts,$(TEST_HC_OPTS))' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst'  $(VANILLA) $(PROF) $(DYN) --disable-optimisation

recompPkgLink:
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	# build libA
	cp packageA/src/LibA1.hs packageA/src/LibA.hs
	rm -rf packageA/dist
	(cd packageA; $(CONFIGURE) --ipid "p-0.1")
	(cd packageA; $(SETUP) build)
	(cd packageA; $(SETUP) copy)
	(cd packageA; $(SETUP) register)
	# build libB
	rm -rf packageB/dist
	(cd packageB; $(CONFIGURE) --ipid "q-0.1")
	(cd packageB; $(SETUP) build)
	(cd packageB; $(SETUP) copy)
	(cd packageB; $(SETUP) register)
	./inst/bin/progB
	cp packageA/src/LibA2.hs packageA/src/LibA.hs
	(cd packageA; $(SETUP) build)
	(cd packageA; $(SETUP) copy)
	(cd packageA; $(SETUP) register)
	(cd packageB; $(SETUP) build)
	(cd packageB; $(SETUP) copy)
	(cd packageB; $(SETUP) register)
	./inst/bin/progB
