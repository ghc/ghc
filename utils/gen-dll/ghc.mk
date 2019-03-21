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

utils/gen-dll_USES_CABAL           = YES
utils/gen-dll_PACKAGE              = gen-dll
utils/gen-dll_dist_PROGNAME        = gen-dll
utils/gen-dll_dist_INSTALL         = NO
utils/gen-dll_dist_INSTALL_INPLACE = YES

$(eval $(call build-prog,utils/gen-dll,dist,0))
