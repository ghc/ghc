driver/mangler_PERL_SRC  = ghc-asm.lprl
driver/mangler_dist_PROG = $(GHC_MANGLER_PGM)
driver/mangler_dist_LIBEXEC = YES

$(eval $(call build-perl,driver/mangler,dist))

INSTALL_LIBEXEC_SCRIPTS += driver/mangler/dist/$(GHC_MANGLER_PGM)
