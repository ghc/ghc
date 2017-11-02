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

utils/check-ppr_USES_CABAL                   = YES
utils/check-ppr_PACKAGE                      = check-ppr
utils/check-ppr_dist-install_PROGNAME        = check-ppr
utils/check-ppr_dist-install_INSTALL         = NO
utils/check-ppr_dist-install_INSTALL_INPLACE = YES
$(eval $(call build-prog,utils/check-ppr,dist-install,2))
