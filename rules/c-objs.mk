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

define c-objs  # args: $1 = dir, $2 = distdir, $3 = way
# C, C++, and S files are usually only built for way "v", but sometimes "dyn" too
$1_$2_$3_C_OBJS   = $$(patsubst %.c,$1/$2/build/%.$$($3_osuf),$$($1_$2_C_SRCS))
$1_$2_$3_CXX_OBJS = $$(patsubst %.cpp,$1/$2/build/%.$$($3_osuf),$$($1_$2_CXX_SRCS))
$1_$2_$3_S_OBJS   = $$(patsubst %.S,$1/$2/build/%.$$($3_osuf),$$($1_$2_S_SRCS))
endef
