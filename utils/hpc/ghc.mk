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

utils/hpc_USES_CABAL                   = YES
utils/hpc_PACKAGE                      = hpc-bin
utils/hpc_dist-install_INSTALL         = YES
utils/hpc_dist-install_INSTALL_INPLACE = YES
utils/hpc_dist-install_PROGNAME        = hpc
$(eval $(call build-prog,utils/hpc,dist-install,1))
