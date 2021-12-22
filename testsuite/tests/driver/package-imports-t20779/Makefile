TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure --with-ghc='$(TEST_HC)' --ghc-options='$(filter-out -rtsopts,$(TEST_HC_OPTS))' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst'

package-imports-20779:
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	# build q1
	rm -rf q-1/dist
	(cd q-1; $(CONFIGURE) --ipid "q-1")
	(cd q-1; $(SETUP) build)
	(cd q-1; $(SETUP) copy)
	(cd q-1; $(SETUP) register)

	# build q2
	rm -rf q-2/dist
	(cd q-2; $(CONFIGURE) --ipid "q-2")
	(cd q-2; $(SETUP) build)
	(cd q-2; $(SETUP) copy)
	(cd q-2; $(SETUP) register)

	# build q3
	rm -rf q-3/dist
	(cd q-3; $(CONFIGURE) --ipid "q-3")
	(cd q-3; $(SETUP) build)
	(cd q-3; $(SETUP) copy)
	(cd q-3; $(SETUP) register)

	# build p
	rm -rf p/dist
	(cd p; $(CONFIGURE) --ipid "p-1")
	(cd p; $(SETUP) build)
	(cd p; $(SETUP) copy)
	(cd p; $(SETUP) register)



