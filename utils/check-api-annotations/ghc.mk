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

utils/check-api-annotations_USES_CABAL                   = YES
utils/check-api-annotations_PACKAGE                      = check-api-annotations
utils/check-api-annotations_dist-install_PROGNAME        = check-api-annotations
utils/check-api-annotations_dist-install_INSTALL         = NO
utils/check-api-annotations_dist-install_INSTALL_INPLACE = YES
$(eval $(call build-prog,utils/check-api-annotations,dist-install,2))
