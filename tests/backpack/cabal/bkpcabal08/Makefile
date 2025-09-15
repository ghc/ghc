TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=./Setup -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --with-hc-pkg='$(GHC_PKG)' --ghc-options='$(TEST_HC_OPTS)' --package-db=tmp.d --prefix='$(PWD)/inst'

bkpcabal08: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	# typecheck
	$(CONFIGURE)
	$(SETUP) build
	$(SETUP) -v1 build
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -r tmp.d inst dist Setup$(exeext)
