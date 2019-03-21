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

utils/check-ppr_USES_CABAL                   = YES
utils/check-ppr_PACKAGE                      = check-ppr
utils/check-ppr_dist-install_PROGNAME        = check-ppr
utils/check-ppr_dist-install_INSTALL         = NO
utils/check-ppr_dist-install_INSTALL_INPLACE = YES
$(eval $(call build-prog,utils/check-ppr,dist-install,2))
