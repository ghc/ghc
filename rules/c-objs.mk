# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

define c-objs  # args: $1 = dir, $2 = distdir
# C and S files are built only once, not once per way
$1_$2_C_OBJS = $$(patsubst %.c,$1/$2/build/%.$$(v_osuf),$$($1_$2_C_SRCS))
$1_$2_S_OBJS = $$(patsubst %.S,$1/$2/build/%.$$(v_osuf),$$($1_$2_S_SRCS))
endef
