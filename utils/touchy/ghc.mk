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

utils/touchy_dist_C_SRCS          = touchy.c
utils/touchy_dist_PROG            = touchy$(exeext)
utils/touchy_dist_TOPDIR          = YES
utils/touchy_dist_INSTALL         = YES
utils/touchy_dist_INSTALL_INPLACE = YES
$(eval $(call build-prog,utils/touchy,dist,0))
