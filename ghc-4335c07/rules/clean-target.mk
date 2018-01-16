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


define clean-target # args: $1 = dir, $2 = key, $3 = files/dirs to clean
clean : clean_$1
.PHONY: clean_$1
clean_$1 : clean_$1_$2
.PHONY: clean_$1_$2
clean_$1_$2:
	$$(call removeTrees,$3)
endef
