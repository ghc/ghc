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

driver/mangler_PERL_SRC  = ghc-asm.lprl
driver/mangler_dist_PROG = $(GHC_MANGLER_PGM)
driver/mangler_dist_TOPDIR = YES

$(eval $(call build-perl,driver/mangler,dist))

INSTALL_TOPDIR_SCRIPTS += driver/mangler/dist/$(GHC_MANGLER_PGM)
