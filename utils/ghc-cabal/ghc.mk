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
CABAL_DOTTED_VERSION := $(shell grep "^Version:" libraries/Cabal/Cabal.cabal | sed "s/^Version: //")
CABAL_VERSION := $(subst .,$(comma),$(CABAL_DOTTED_VERSION))
CABAL_CONSTRAINT := --constraint="Cabal == $(CABAL_DOTTED_VERSION)"

$(GHC_CABAL_INPLACE) : $(GHC_CABAL_DIR)/dist/build/tmp/ghc-cabal$(exeext)
	$(MKDIRHIER) $(dir $@)
	$(CP) $< $@

$(GHC_CABAL_DIR)/dist/build/tmp/ghc-cabal$(exeext): $(GHC_CABAL_DIR)/ghc-cabal.hs $(MKDIRHIER)
	$(MKDIRHIER) bootstrapping
	$(MKDIRHIER) $(dir $@)
	$(GHC) --make $(GHC_CABAL_DIR)/ghc-cabal.hs -o $@ \
	       -Wall $(WERROR) \
	       -DCABAL_VERSION=$(CABAL_VERSION) \
	       -odir  bootstrapping \
	       -hidir bootstrapping \
	       -ilibraries/Cabal \
	       -ilibraries/filepath \
	       -ilibraries/hpc
	touch $@

# touch is required, because otherwise if mkdirhier is newer, we
# repeatedly rebuild ghc-cabal.

$(eval $(call clean-target,$(GHC_CABAL_DIR),dist,\
   $(GHC_CABAL_DIR)/dist bootstrapping))

$(eval $(call all-target,$(GHC_CABAL_DIR),$(GHC_CABAL_INPLACE)))

# -----------------------------------------------------------------------------
# dummy-ghc

# This is a tiny program to fool Cabal's configure that we have a
# stage1 GHC, which lets us configure all the packages before we've
# build stage1.

$(GHC_CABAL_DIR)_dist-dummy-ghc_MODULES = dummy-ghc
$(GHC_CABAL_DIR)_dist-dummy-ghc_PROG    = dummy-ghc$(exeext)

$(GHC_CABAL_DIR)/dist-dummy-ghc/build/dummy-ghc.hs : $(GHC_CABAL_DIR)/ghc.mk $(MKDIRHIER)
	$(MKDIRHIER) $(dir $@)
	echo "import System.Environment; import System.Cmd; import System.Exit" >$@
	echo "main = do args <- getArgs; if args == [\"--numeric-version\"] then putStrLn \"$(ProjectVersion)\" else do e <- rawSystem \"$(GHC_STAGE0)\" args; exitWith e" >>$@

# We don't build dummy-ghc with Cabal, so we need to pass -package
# flags manually
utils/ghc-cabal_dist-dummy-ghc_HC_OPTS = -package process
$(eval $(call build-prog,utils/ghc-cabal,dist-dummy-ghc,0))

