driver/split_PERL_SRC  = ghc-split.lprl
driver/split_dist_PROG = $(GHC_SPLIT_PGM)
driver/split_dist_LIBEXEC = YES

$(eval $(call build-perl,driver/split,dist))

INSTALL_LIBEXEC_SCRIPTS += driver/split/dist/$(GHC_SPLIT_PGM)
