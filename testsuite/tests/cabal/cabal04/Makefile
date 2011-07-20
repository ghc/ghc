TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=./Setup -v0

# This tests that we can build a Cabal package that uses TH with both
# profiling and dynamic linking. (#3604)

cabal04:
	$(MAKE) clean
	'$(TEST_HC)' -v0 --make Setup
	$(SETUP) clean
	$(SETUP) configure --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' $(PROF) $(DYN)
	$(SETUP) build 2> err
	! grep -v "Creating library file" err
ifneq "$(CLEANUP)" ""
	$(MAKE) clean
endif

clean :
	$(RM) -r *.o *.hi dist Setup$(exeext) err

