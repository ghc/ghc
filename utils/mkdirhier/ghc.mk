# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
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
