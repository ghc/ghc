TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=../Setup -v0
CONFIGURE=$(SETUP) configure --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst'

T13168: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup

	cd package1 && $(CONFIGURE) && $(SETUP) build && $(SETUP) copy && $(SETUP) register
	cd package2 && $(CONFIGURE) && $(SETUP) build && $(SETUP) copy && $(SETUP) register

	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -package-db tmp.d < T13168.script

clean :
	$(RM) -r tmp.d inst dist Setup$(exeext)
