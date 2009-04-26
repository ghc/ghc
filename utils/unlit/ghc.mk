utils/unlit_dist_C_SRCS  = unlit.c
utils/unlit_dist_PROG    = $(GHC_UNLIT_PGM)
utils/unlit_dist_LIBEXEC = YES
utils/unlit_dist_INSTALL = YES

ifneq "$(BINDIST)" "YES"
$(eval $(call build-prog,utils/unlit,dist,0))
endif

