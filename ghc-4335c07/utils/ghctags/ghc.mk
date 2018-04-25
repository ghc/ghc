# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

utils/ghctags_USES_CABAL                   = YES
utils/ghctags_PACKAGE                      = ghctags
utils/ghctags_dist-install_PROGNAME        = ghctags
utils/ghctags_dist-install_INSTALL         = NO
utils/ghctags_dist-install_INSTALL_INPLACE = YES
$(eval $(call build-prog,utils/ghctags,dist-install,2))
