TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst'

remote-iserv: clean
	'$(GHC_PKG)' init tmp.d

	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup

	cp -r $(TOP)/../libraries/libiserv .
	cd libiserv && $(CONFIGURE) -fnetwork
	cd libiserv && $(SETUP) build
	cd libiserv && $(SETUP) copy
	cd libiserv && $(SETUP) register

	cp -r $(TOP)/../utils/iserv-proxy .
	cd iserv-proxy && $(CONFIGURE)
	cd iserv-proxy && $(SETUP) build
	cd iserv-proxy && $(SETUP) copy
	cd iserv-proxy && $(SETUP) register

	cp -r $(TOP)/../utils/remote-iserv .
	cd remote-iserv && $(CONFIGURE)
	cd remote-iserv && $(SETUP) build
	cd remote-iserv && $(SETUP) copy
	cd remote-iserv && $(SETUP) register

	'$(TEST_HC)' $(TEST_HC_OPTS)  -fexternal-interpreter -pgmi $(PWD)/iserv-wrapper Main.hs

ifneq "$(CLEANUP)" ""
        $(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -rf tmp *.o *.hi Main libiserv iserv-proxy remote-iserv tmp.d inst dist Setup$(exeext)
