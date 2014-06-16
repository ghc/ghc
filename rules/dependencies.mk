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

define dependencies
$(call trace, dependencies($1,$2,$3))
$(call profStart, dependencies($1,$2,$3))
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

# We always have the dependency rules available, as we need to know
# how to build hsc2hs's dependency file in phase 0
$(call build-dependencies,$1,$2,$3)

ifneq "$(phase)" "0"
# From phase 1 we actually include the dependency files for the
# bootstrapping stuff
ifeq "$3" "0"
$(call include-dependencies,$1,$2,$3)
else ifeq "$(phase)" "final"
# In the final phase, we also include the dependency files for
# everything else
$(call include-dependencies,$1,$2,$3)
endif
endif

$(call profEnd, dependencies($1,$2,$3))
endef

