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

utils/mkUserGuidePart_dist_MODULES = Main
utils/mkUserGuidePart_dist_PROG    = mkUserGuidePart$(exeext)
utils/mkUserGuidePart_HC_OPTS      = -package ghc

utils/mkUserGuidePart/dist/build/Main.o: $(ALL_STAGE1_LIBS) $(compiler_stage2_v_LIB)

$(eval $(call build-prog,utils/mkUserGuidePart,dist,1))
