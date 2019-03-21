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

# built by ghc-stage0
utils/unlit_dist_C_SRCS  = unlit.c fs.c
utils/unlit_dist_PROGNAME = unlit
utils/unlit_dist_TOPDIR  = YES
utils/unlit_dist_INSTALL_INPLACE = YES

# built by ghc-stage1
utils/unlit_dist-install_C_SRCS = $(utils/unlit_dist_C_SRCS)
utils/unlit_dist-install_PROGNAME = $(utils/unlit_dist_PROGNAME)
utils/unlit_dist-install_TOPDIR = $(utils/unlit_dist_TOPDIR)
utils/unlit_dist-install_INSTALL_INPLACE = NO

ifeq "$(Stage1Only)" "YES"
utils/unlit_dist_INSTALL         = YES
utils/unlit_dist-install_INSTALL = NO
else
utils/unlit_dist_INSTALL         = NO
utils/unlit_dist-install_INSTALL = YES
endif

$(eval $(call build-prog,utils/unlit,dist,0))
$(eval $(call build-prog,utils/unlit,dist-install,1))
