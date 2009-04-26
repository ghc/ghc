utils/hp2ps_dist_C_SRCS = AreaBelow.c Curves.c Error.c Main.c \
                          Reorder.c TopTwenty.c AuxFile.c Deviation.c \
                          HpFile.c Marks.c Scale.c TraceElement.c \
                          Axes.c Dimensions.c Key.c PsFile.c Shade.c \
                          Utilities.c
utils/hp2ps_dist_PROG    = hp2ps$(exeext)
utils/hp2ps_dist_INSTALL = YES

$(eval $(call build-prog,utils/hp2ps,dist,0))

