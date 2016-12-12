TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst'

bkpcabal03: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	cp bkpcabal03.cabal.in1 bkpcabal03.cabal
	# typecheck asig1
	(cd asig1; $(CONFIGURE) --cid "asig1" asig1)
	(cd asig1; $(SETUP) build)
	(cd asig1; $(SETUP) copy)
	(cd asig1; $(SETUP) register)
	# typecheck asig2
	(cd asig2; $(CONFIGURE) --cid "asig2" asig2)
	(cd asig2; $(SETUP) build)
	(cd asig2; $(SETUP) copy)
	(cd asig2; $(SETUP) register)
	# typecheck top-level
	$(CONFIGURE) --cid "toplevel" bkpcabal03
	! $(SETUP) build
	# modify mixins
	cp bkpcabal03.cabal.in2 bkpcabal03.cabal
	# retypecheck top-level
	$(CONFIGURE) --cid "toplevel" bkpcabal03
	$(SETUP) build
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -r tmp.d inst dist Setup$(exeext)
