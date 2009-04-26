ifeq "$(Windows)" "YES"

driver/ghc_dist_C_SRCS  = ghc.c
driver/ghc_dist_PROG    = ghc-$(ProjectVersion)
driver/ghc_dist_INSTALL = YES

$(eval $(call build-prog,driver/ghc,dist,0))

endif

