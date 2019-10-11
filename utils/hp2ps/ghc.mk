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

utils/hp2ps_CC_OPTS += $(addprefix -I,$(GHC_INCLUDE_DIRS))

# stage0
utils/hp2ps_dist_C_SRCS          = AreaBelow.c Curves.c Error.c Main.c \
                                   Reorder.c TopTwenty.c AuxFile.c Deviation.c \
                                   HpFile.c Marks.c Scale.c TraceElement.c \
                                   Axes.c Dimensions.c Key.c PsFile.c Shade.c \
                                   Utilities.c
utils/hp2ps_dist_EXTRA_LIBRARIES = m
utils/hp2ps_dist_PROGNAME        = hp2ps
utils/hp2ps_dist_INSTALL_INPLACE = YES
utils/hp2ps_dist_SHELL_WRAPPER              = YES
utils/hp2ps_dist_INSTALL_SHELL_WRAPPER_NAME = hp2ps

# stage 1
utils/hp2ps_dist-install_C_SRCS = $(utils/hp2ps_dist_C_SRCS)
utils/hp2ps_dist-install_EXTRA_LIBRARIES = $(utils/hp2ps_dist_EXTRA_LIBRARIES)
utils/hp2ps_dist-install_PROGNAME        = $(utils/hp2ps_dist_PROGNAME)
utils/hp2ps_dist-install_INSTALL_INPLACE = NO
utils/hp2ps_dist-install_SHELL_WRAPPER              = YES
utils/hp2ps_dist-install_INSTALL_SHELL_WRAPPER_NAME = $(utils/hp2ps_dist_INSTALL_SHELL_WRAPPER_NAME)

ifeq "$(Stage1Only)" "YES"
utils/hp2ps_dist_INSTALL         = YES
utils/hp2ps_dist-install_INSTALL = NO
else
utils/hp2ps_dist_INSTALL         = NO
utils/hp2ps_dist-install_INSTALL = YES
endif

$(eval $(call build-prog,utils/hp2ps,dist,0))
$(eval $(call build-prog,utils/hp2ps,dist-install,1))
