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

define c-sources  # args: $1 = dir, $2 = distdir
$1_$2_C_FILES   = $$(patsubst %,$1/%,$$($1_$2_C_SRCS))
$1_$2_CXX_FILES = $$(patsubst %,$1/%,$$($1_$2_CXX_SRCS))
$1_$2_S_FILES   = $$(patsubst %,$1/%,$$($1_$2_S_SRCS))
$1_$2_CMM_FILES = $$(patsubst %,$1/%,$$($1_$2_CMM_SRCS))
endef
