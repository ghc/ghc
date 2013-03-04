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

driver/split_PERL_SRC  = ghc-split.lprl
driver/split_dist_PROGNAME = ghc-split
driver/split_dist_TOPDIR = YES
driver/split_dist_INSTALL = YES
driver/split_dist_INSTALL_INPLACE = YES

$(eval $(call build-perl,driver/split,dist))

