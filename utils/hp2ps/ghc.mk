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

utils/hp2ps_dist_C_SRCS          = AreaBelow.c Curves.c Error.c Main.c \
                                   Reorder.c TopTwenty.c AuxFile.c Deviation.c \
                                   HpFile.c Marks.c Scale.c TraceElement.c \
                                   Axes.c Dimensions.c Key.c PsFile.c Shade.c \
                                   Utilities.c
utils/hp2ps_dist_EXTRA_LIBRARIES = m
utils/hp2ps_dist_PROGNAME        = $(CrossCompilePrefix)hp2ps
utils/hp2ps_dist_INSTALL         = YES
utils/hp2ps_dist_INSTALL_INPLACE = YES

utils/hp2ps_CC_OPTS += $(addprefix -I,$(GHC_INCLUDE_DIRS))

$(eval $(call build-prog,utils/hp2ps,dist,0))

