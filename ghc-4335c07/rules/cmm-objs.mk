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

define cmm-objs  # args: $1 = dir, $2 = distdir, $3 = way

$1_$2_$3_CMM_OBJS = $$(patsubst %.cmm,$1/$2/build/%.$$($3_osuf),$$($1_$2_CMM_SRCS))
endef
