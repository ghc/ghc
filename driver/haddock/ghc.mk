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

ifeq "$(Windows_Host)" "YES"

driver/haddock_dist_C_SRCS   = haddock.c ../utils/cwrapper.c ../utils/getLocation.c
driver/haddock_dist_CC_OPTS += -I driver/utils
driver/haddock_dist_PROGNAME = haddock-$(ProjectVersion)
driver/haddock_dist_INSTALL  = YES
driver/haddock_dist_INSTALL_INPLACE = NO

$(eval $(call build-prog,driver/haddock,dist,0))

endif

