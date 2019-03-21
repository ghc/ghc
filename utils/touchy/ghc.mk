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

utils/touchy_dist_C_SRCS          = touchy.c
utils/touchy_dist_PROGNAME        = touchy
utils/touchy_dist_TOPDIR          = YES
utils/touchy_dist_INSTALL         = YES
utils/touchy_dist_INSTALL_INPLACE = YES
$(eval $(call build-prog,utils/touchy,dist,0))
