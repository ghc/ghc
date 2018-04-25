TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP='$(PWD)/Setup' -v0
CONFIGURE=$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --package-db='$(PWD)/tmp.d' --prefix='$(PWD)/inst'

# This test checks if recompilation works correctly when we change an
# hsig file which modifies the set of exported instances.  Makes sure
# we track dependencies on instances from signatures correctly.

bkpcabal06: clean
	$(MAKE) -s --no-print-directory clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' -v0 --make Setup
	$(CONFIGURE)
	cp impl/P.hs.in1 impl/P.hs
	$(SETUP) build
	sleep 1
	cp impl/P.hs.in2 impl/P.hs
	! $(SETUP) build
ifneq "$(CLEANUP)" ""
	$(MAKE) -s --no-print-directory clean
endif

clean :
	$(RM) -rf tmp.d inst dist Setup$(exeext) impl/P.hs
