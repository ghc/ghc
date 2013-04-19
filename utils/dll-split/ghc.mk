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

utils/dll-split_USES_CABAL                   = YES
utils/dll-split_PACKAGE                      = dll-split
utils/dll-split_dist-install_PROGNAME        = dll-split
utils/dll-split_dist-install_INSTALL         = NO
utils/dll-split_dist-install_INSTALL_INPLACE = YES
$(eval $(call build-prog,utils/dll-split,dist-install,1))
