TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst'

amazonka-ec2-prep:
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	# build amazonka-core
	rm -rf core/dist
	(cd core; $(CONFIGURE) --ipid "amazonka-core-0.1")
	(cd core; $(SETUP) build)
	(cd core; $(SETUP) copy)
	(cd core; $(SETUP) register)
