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


define clean-target # args: $1 = dir, $2 = key, $3 = files/dirs to clean
clean : clean_$1
.PHONY: clean_$1
clean_$1 : clean_$1_$2
.PHONY: clean_$1_$2
clean_$1_$2:
	$$(call removeTrees,$3)
endef
