TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(filter-out -rtsopts,$(TEST_HC_OPTS))' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst' $(VANILLA) $(PROF) $(DYN)

recompChangedPackage:
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	# build q
	(cd q; cp q.cabal1 q.cabal)
	rm -rf q/dist
	(cd q; $(CONFIGURE) --ipid "q-0.1")
	(cd q; $(SETUP) build)
	(cd q; $(SETUP) copy)
	(cd q; $(SETUP) register)

	cp PLib1.hs PLib.hs
	'$(TEST_HC)' -package-db tmp.d Main.hs
	./Main

	# Now add PLib to q.. Main should be recompiled

	'$(GHC_PKG)' --package-db tmp.d list
	'$(GHC_PKG)' --package-db tmp.d unregister q-0.1.0.0
	rm PLib.hs

	(cd q; cp q.cabal2 q.cabal)
	rm -rf q/dist
	(cd q; $(CONFIGURE) --ipid "q-0.1")
	(cd q; $(SETUP) build)
	(cd q; $(SETUP) copy)
	(cd q; $(SETUP) register)

	'$(TEST_HC)' -package-db tmp.d Main.hs
	./Main
