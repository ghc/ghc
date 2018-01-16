TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=./Setup -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db=tmp.d --prefix='$(PWD)/inst'

bkpcabal02: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	# Here is a working signature which this
	# package can typecheck with
	cp p/H.hsig.in1 p/H.hsig
	# typecheck everything
	$(CONFIGURE)
	$(SETUP) build
	$(SETUP) -v1 build
	# This signature doesn't typecheck with the package
	cp p/H.hsig.in2 p/H.hsig
	! $(SETUP) build
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -r tmp.d inst dist Setup$(exeext)
