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

utils/count-deps_USES_CABAL                   = YES
utils/count-deps_PACKAGE                      = count-deps
utils/count-deps_dist-install_PROGNAME        = count-deps
utils/count-deps_dist-install_INSTALL         = NO
utils/count-deps_dist-install_INSTALL_INPLACE = YES
$(eval $(call build-prog,utils/count-deps,dist-install,2))
