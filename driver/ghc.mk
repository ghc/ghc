# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------

$(eval $(call all-target,driver,$(INPLACE_LIB)/ghc-usage.txt $(INPLACE_LIB)/ghci-usage.txt))

$(INPLACE_LIB)/ghc-usage.txt: driver/ghc-usage.txt
	cp $< $@

$(INPLACE_LIB)/ghci-usage.txt: driver/ghci-usage.txt
	cp $< $@

INSTALL_LIBS += driver/ghc-usage.txt driver/ghci-usage.txt

