# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

utils/deriveConstants_dist_MODULES = DeriveConstants
utils/deriveConstants_dist_PROGNAME = deriveConstants
utils/deriveConstants_dist_INSTALL_INPLACE = YES
utils/deriveConstants_HC_OPTS += -package process -package containers

$(eval $(call build-prog,utils/deriveConstants,dist,0))

