TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst'

bkpcabal04: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	cp bkpcabal04.cabal.in1 bkpcabal04.cabal
	cp q/B.hsig.in q/B.hsig
	# typecheck
	$(CONFIGURE)
	$(SETUP) build
	# new version
	cp bkpcabal04.cabal.in2 bkpcabal04.cabal
	rm q/B.hsig
	cp q/A/B.hsig.in q/A/B.hsig
	# typecheck
	$(CONFIGURE)
	$(SETUP) build
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -rf tmp.d inst dist Setup$(exeext) q/A/B.hsig q/B.hsig bkpcabal04.cabal
