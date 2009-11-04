TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=./Setup -v0

# This tests that we can build a Cabal package that uses TH with both
# profiling and dynamic linking. (#3604)

cabal04:
	'$(TEST_HC)' -v0 --make Setup
	$(SETUP) clean
	$(SETUP) configure --with-ghc='$(TEST_HC)' $(PROF) $(DYN)
	$(SETUP) build
