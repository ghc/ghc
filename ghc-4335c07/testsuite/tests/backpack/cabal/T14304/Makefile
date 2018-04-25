TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst' --enable-shared

T14304: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	# typecheck indef
	rm -rf indef/dist
	(cd indef; $(CONFIGURE) --ipid "indef-0.1")
	(cd indef; $(SETUP) build)
	(cd indef; $(SETUP) copy)
	(cd indef; $(SETUP) register)
	# build p
	rm -rf p/dist
	(cd p; $(CONFIGURE) --ipid "p-0.1")
	(cd p; $(SETUP) build)
	(cd p; $(SETUP) copy)
	(cd p; $(SETUP) register)
	# build indef instantiated with p
	rm -rf indef/dist
	(cd indef; $(CONFIGURE) --ipid "indef-0.1" --instantiate-with "Sig=p-0.1:P")
	(cd indef; $(SETUP) build)
	(cd indef; $(SETUP) copy)
	(cd indef; $(SETUP) register)
	# build th (which tests if we have correct linkage)
	rm -rf th/dist
	(cd th; $(CONFIGURE))
	(cd th; $(SETUP) build)

clean :
	$(RM) -r tmp.d inst dist Setup$(exeext)
