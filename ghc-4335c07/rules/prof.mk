# -----------------------------------------------------------------------------
#
# (c) 2010 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

define profStart
$$(if $(PROF),$$(info $$(shell date +%s.%N): Start $1))
endef

define profEnd
$$(if $(PROF),$$(info $$(shell date +%s.%N): End   $1))
endef

