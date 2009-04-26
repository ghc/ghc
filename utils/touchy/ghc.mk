utils/touchy_dist_C_SRCS  = touchy.c
utils/touchy_dist_PROG    = $(GHC_TOUCHY_PGM)
utils/touchy_dist_LIBEXEC = YES
utils/touchy_dist_INSTALL = YES
$(eval $(call build-prog,utils/touchy,dist,0))
