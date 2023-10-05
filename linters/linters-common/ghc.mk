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

linters/linters-common_USES_CABAL                   = YES
linters/linters-common_PACKAGE                      = linters-common
linters/linters-common_dist-install_INSTALL         = YES
linters/linters-common_dist-install_INSTALL_INPLACE = YES
$(eval $(call build-package,linters/linters-common,dist-install,1))
