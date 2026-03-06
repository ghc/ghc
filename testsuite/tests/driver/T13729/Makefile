TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --with-hc-pkg='$(GHC_PKG)' --ghc-options='$(filter-out -rtsopts,$(TEST_HC_OPTS))' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst' $(VANILLA) $(PROF) $(DYN)

T13729:
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup

	# Build package A with the first Foo definition.
	rm -rf A/dist
	cp A/Types1.hs A/Types.hs
	(cd A; $(CONFIGURE) --ipid "A-0.1")
	(cd A; $(SETUP) build)
	(cd A; $(SETUP) copy)
	(cd A; $(SETUP) register)

	# Build package B and record the splice output.
	rm -rf B/dist
	(cd B; $(CONFIGURE) --ipid "B-0.1")
	(cd B; $(SETUP) build)
	(cd B; $(SETUP) copy)
	(cd B; $(SETUP) register)
	./inst/bin/B > out1
	grep -q Bar out1
	! grep -q Baz out1

	# Overwrite Types.hs with the second Foo definition, rebuild A, then rebuild B
	# without -fforce-recomp.
	cp A/Types2.hs A/Types.hs
	(cd A; $(SETUP) build)
	(cd A; $(SETUP) copy)
	(cd A; $(SETUP) register)
	(cd B; $(SETUP) build)
	(cd B; $(SETUP) copy)
	(cd B; $(SETUP) register)
	./inst/bin/B > out2
	grep -q Baz out2
