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

ifeq "$(Windows_Host)" "YES"

driver/ghc_dist_C_SRCS   = ghc.c ../utils/cwrapper.c ../utils/getLocation.c
driver/ghc_dist_CC_OPTS += -I driver/utils
driver/ghc_dist_PROGNAME = ghc-$(ProjectVersion)
driver/ghc_dist_INSTALL  = YES
driver/ghc_dist_INSTALL_INPLACE = NO

$(eval $(call build-prog,driver/ghc,dist,0))

endif

