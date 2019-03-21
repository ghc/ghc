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

utils/genprimopcode_USES_CABAL           = YES
utils/genprimopcode_PACKAGE              = genprimopcode
utils/genprimopcode_dist_PROGNAME        = genprimopcode
utils/genprimopcode_dist_INSTALL         = NO
utils/genprimopcode_dist_INSTALL_INPLACE = YES

$(eval $(call build-prog,utils/genprimopcode,dist,0))
