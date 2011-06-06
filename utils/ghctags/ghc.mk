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

utils/ghctags_dist-install_MODULES = Main
utils/ghctags_dist-install_HC_OPTS = -package ghc
utils/ghctags_dist-install_INSTALL = NO
utils/ghctags_dist-install_PROG    = ghctags$(exeext)
$(eval $(call build-prog,utils/ghctags,dist-install,2))
