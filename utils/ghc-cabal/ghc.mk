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
CABAL_DOTTED_VERSION := $(shell grep "^version:" libraries/Cabal/Cabal/Cabal.cabal | sed "s/^version: //")
CABAL_VERSION := $(subst .,$(comma),$(CABAL_DOTTED_VERSION))
CABAL_CONSTRAINT := --constraint="Cabal == $(CABAL_DOTTED_VERSION)"

ghc-cabal_DIST_BINARY = utils/ghc-cabal/dist/build/tmp/ghc-cabal$(exeext)
ghc-cabal_INPLACE = inplace/bin/ghc-cabal$(exeext)

ifneq "$(BINDIST)" "YES"
$(ghc-cabal_INPLACE) : $(ghc-cabal_DIST_BINARY) | $$(dir $$@)/.
	"$(CP)" $< $@

$(ghc-cabal_DIST_BINARY): $(wildcard libraries/Cabal/Cabal/Distribution/*/*/*.hs)
$(ghc-cabal_DIST_BINARY): $(wildcard libraries/Cabal/Cabal/Distribution/*/*.hs)
$(ghc-cabal_DIST_BINARY): $(wildcard libraries/Cabal/Cabal/Distribution/*.hs)

$(ghc-cabal_DIST_BINARY): utils/ghc-cabal/Main.hs $(TOUCH_DEP) | $$(dir $$@)/. bootstrapping/.
	"$(GHC)" $(SRC_HC_OPTS) --make utils/ghc-cabal/Main.hs -o $@ \
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
endif

# touch is required, because otherwise if mkdirhier is newer, we
# repeatedly rebuild ghc-cabal.

$(eval $(call clean-target,utils/ghc-cabal,dist,\
   utils/ghc-cabal/dist bootstrapping))

$(eval $(call all-target,utils/ghc-cabal,$(ghc-cabal_INPLACE)))

# -----------------------------------------------------------------------------
# Now make another copy that goes in bindists. This needs to be built
# with the in-tree compiler.

utils/ghc-cabal_USES_CABAL                   = YES
utils/ghc-cabal_PACKAGE                      = ghc-cabal
utils/ghc-cabal_dist-install_PROGNAME        = ghc-cabal
utils/ghc-cabal_dist-install_INSTALL_INPLACE = NO
utils/ghc-cabal_dist-install_WANT_BINDIST_WRAPPER = YES
utils/ghc-cabal_dist-install_MODULES         = Main

$(eval $(call build-prog,utils/ghc-cabal,dist-install,1))

