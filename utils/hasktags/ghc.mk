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

utils/hasktags_dist_MODULES = Main
utils/hasktags_dist_PROG    = hasktags$(exeext)
utils/hasktags_dist_INSTALL = YES

utils/hasktags/dist/build/Main.hs : utils/hasktags/HaskTags.hs $(MKDIRHIER)
	$(MKDIRHIER) $(dir $@)
	$(CP) $< $@

$(eval $(call build-prog,utils/hasktags,dist,1))

