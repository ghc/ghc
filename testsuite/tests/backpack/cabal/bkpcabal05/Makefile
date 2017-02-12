TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst'

bkpcabal05: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	$(CONFIGURE)
	cp A.hsig.in1 A.hsig
	# typecheck
	$(SETUP) build
	cp A.hsig.in2 A.hsig
	! $(SETUP) build
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -rf tmp.d inst dist Setup$(exeext) A.hsig
