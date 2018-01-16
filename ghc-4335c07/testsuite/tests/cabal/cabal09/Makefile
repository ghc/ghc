TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP = ./Setup -v0

# This test is for package reexports from a wired-in package,
# which had a bug

cabal09: clean
	$(MAKE) clean
	'$(TEST_HC)' -v0 --make Setup
	$(SETUP) clean
	$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)'
	$(SETUP) build
ifneq "$(CLEANUP)" ""
	$(MAKE) clean
endif

clean :
	$(RM) -r */dist Setup$(exeext) *.o *.hi
