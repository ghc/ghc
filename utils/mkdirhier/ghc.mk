# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

$(MKDIRHIER) : utils/mkdirhier/mkdirhier.sh
	-mkdir $(INPLACE)
	-mkdir $(INPLACE_BIN)
	-mkdir $(INPLACE_LIB)
	$(call removeFiles,$@)
	echo '#!$(SHELL)'  		 >> $@
	cat utils/mkdirhier/mkdirhier.sh >> $@
	$(EXECUTABLE_FILE) $@

$(eval $(call all-target,utils/mkdirhier,$(MKDIRHIER)))
$(eval $(call clean-target,utils/mkdirhier,,$(MKDIRHIER)))
