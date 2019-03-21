# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------

utils/deriveConstants_USES_CABAL           = YES
utils/deriveConstants_PACKAGE              = deriveConstants
utils/deriveConstants_dist_PROGNAME        = deriveConstants
utils/deriveConstants_dist_INSTALL         = NO
utils/deriveConstants_dist_INSTALL_INPLACE = YES

$(eval $(call build-prog,utils/deriveConstants,dist,0))

