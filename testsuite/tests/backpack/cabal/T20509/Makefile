TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=./Setup -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=tmp.d --prefix='$(PWD)/inst'

T20509: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	# typecheck p
	$(CONFIGURE) --cid "p-0.1" p
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
	# typecheck q
	$(CONFIGURE) --cid "q-0.1" q
	$(SETUP) build
	$(SETUP) copy
	$(SETUP) register
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -r tmp.d inst dist Setup$(exeext)
