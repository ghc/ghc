# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

# Bootstrapping ghc-cabal

# Euch, hideous hack:
# XXX This should be in a different Makefile
CABAL_DOTTED_VERSION := $(shell grep "^Version:" libraries/Cabal/Cabal/Cabal.cabal | sed "s/^Version: //")
CABAL_VERSION := $(subst .,$(comma),$(CABAL_DOTTED_VERSION))
CABAL_CONSTRAINT := --constraint="Cabal == $(CABAL_DOTTED_VERSION)"

$(GHC_CABAL_INPLACE) : $(GHC_CABAL_DIR)/dist/build/tmp/ghc-cabal$(exeext) | $$(dir $$@)/.
	"$(CP)" $< $@

$(GHC_CABAL_DIR)/dist/build/tmp/ghc-cabal$(exeext): $(wildcard libraries/Cabal/Cabal/Distribution/*/*/*.hs)
$(GHC_CABAL_DIR)/dist/build/tmp/ghc-cabal$(exeext): $(wildcard libraries/Cabal/Cabal/Distribution/*/*.hs)
$(GHC_CABAL_DIR)/dist/build/tmp/ghc-cabal$(exeext): $(wildcard libraries/Cabal/Cabal/Distribution/*.hs)

$(GHC_CABAL_DIR)/dist/build/tmp/ghc-cabal$(exeext): $(GHC_CABAL_DIR)/Main.hs $(TOUCH_DEP) | $$(dir $$@)/. bootstrapping/.
	"$(GHC)" $(SRC_HC_OPTS) --make $(GHC_CABAL_DIR)/Main.hs -o $@ \
	       -no-user-$(GHC_PACKAGE_DB_FLAG) \
	       -Wall -fno-warn-unused-imports -fno-warn-warnings-deprecations \
	       -DCABAL_VERSION=$(CABAL_VERSION) \
               -DBOOTSTRAPPING \
	       -odir  bootstrapping \
	       -hidir bootstrapping \
	       -ilibraries/Cabal/Cabal \
	       -ilibraries/filepath \
	       -ilibraries/hpc \
	       $(utils/ghc-cabal_dist_EXTRA_HC_OPTS)
	"$(TOUCH_CMD)" $@

# touch is required, because otherwise if mkdirhier is newer, we
# repeatedly rebuild ghc-cabal.

$(eval $(call clean-target,$(GHC_CABAL_DIR),dist,\
   $(GHC_CABAL_DIR)/dist bootstrapping))

$(eval $(call all-target,$(GHC_CABAL_DIR),$(GHC_CABAL_INPLACE)))

# -----------------------------------------------------------------------------
# Now make another copy that goes in bindists. This needs to be built
# with the in-tree compiler.

$(GHC_CABAL_DIR)_USES_CABAL                   = YES
$(GHC_CABAL_DIR)_PACKAGE                      = ghc-cabal
$(GHC_CABAL_DIR)_dist-install_PROG            = ghc-cabal$(exeext)
$(GHC_CABAL_DIR)_dist-install_INSTALL_INPLACE = NO
$(GHC_CABAL_DIR)_dist-install_WANT_BINDIST_WRAPPER = YES
$(GHC_CABAL_DIR)_dist-install_MODULES         = Main

$(eval $(call build-prog,utils/ghc-cabal,dist-install,1))

