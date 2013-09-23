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

utils/unlit_dist_C_SRCS  = unlit.c
utils/unlit_dist_PROGNAME = unlit
utils/unlit_dist_TOPDIR  = YES
utils/unlit_dist_INSTALL = YES
utils/unlit_dist_INSTALL_INPLACE = YES

$(eval $(call build-prog,utils/unlit,dist,0))

