TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

SETUP=./Setup -v0

# Reproducer for #27013 (!15899#note_676801).
#
# `composition` depends on neither base nor ghc-internal and uses no known-key
# names AT ALL. We pass -frebindable-known-names just so GHC doesn't force an
# implicit GHC.Essentials dependency (which couldn't be solved because of
# -hide-all-packages). See T27013f for the failure when the flag is omitted.

T27013a: clean
	'$(GHC_PKG)' init tmp.d
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Setup
	$(SETUP) clean
	$(SETUP) configure $(CABAL_MINIMAL_BUILD) --with-ghc='$(TEST_HC)' --with-hc-pkg='$(GHC_PKG)' --ghc-options='$(TEST_HC_OPTS) -frebindable-known-names' --package-db=tmp.d
	$(SETUP) build

clean :
	$(RM) -r tmp.d dist Setup$(exeext) *.o *.hi
