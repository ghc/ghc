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
	mkdir -p $(INPLACE_BIN)
	mkdir -p $(INPLACE_LIB)
	$(call removeFiles,$@)
	echo '#!/bin/sh'  		 >> $@
	cat utils/mkdirhier/mkdirhier.sh >> $@
	$(EXECUTABLE_FILE) $@

$(eval $(call all-target,utils/mkdirhier,$(MKDIRHIER)))
$(eval $(call clean-target,utils/mkdirhier,,$(MKDIRHIER)))
