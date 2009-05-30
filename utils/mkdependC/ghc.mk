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

$(MKDEPENDC) : utils/mkdependC/mkdependC.prl $(MKDIRHIER)
	"$(MKDIRHIER)" $(dir $@)
	"$(RM)" $(RM_OPTS) $@
	echo '#!$(PERL)'                               >> $@
	echo '$$DEFAULT_TMPDIR = "$(DEFAULT_TMPDIR)";' >> $@
	echo '$$CPP            = "$(CPP)";'            >> $@
	echo '$$BUILDPLATFORM  = "$(BUILDPLATFORM)";'  >> $@
	cat utils/mkdependC/mkdependC.prl              >> $@
	$(EXECUTABLE_FILE) $@

$(eval $(call all-target,utils/mkdependC,$(MKDEPENDC)))
$(eval $(call clean-target,utils/mkdependC,,$(MKDEPENDC)))
