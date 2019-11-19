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

utils/genapply_USES_CABAL           = YES
utils/genapply_PACKAGE              = genapply
utils/genapply_dist_PROGNAME        = genapply
utils/genapply_dist_INSTALL         = NO
utils/genapply_dist_INSTALL_INPLACE = YES

utils/dist/package-data.mk : $(includes_1_H_PLATFORM)
utils/dist/package-data.mk : $(includes_1_H_CONFIG)

ifeq "$(GhcUnregisterised)" "YES"
utils/genapply_CONFIGURE_OPTS = --flag unregisterised
endif

$(eval $(call build-prog,utils/genapply,dist,0))

# Purposely do the wrong stage for HOST := TARGET hack.
# See Note [Genapply target as host for RTS macros].
utils/genapply_dist_CC_OPTS += -I,$(BUILD_1_INCLUDE_DIR)
