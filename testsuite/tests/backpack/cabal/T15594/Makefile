TOP=/home/ezyang/Dev/ghc-known-nat/testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst'

T15594: clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	$(CONFIGURE)
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -rf tmp.d inst dist Setup$(exeext)
