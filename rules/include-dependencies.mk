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

define include-dependencies
$(call trace, include-dependencies($1,$2,$3))
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

ifneq "$$(NO_INCLUDE_DEPS)" "YES"
ifneq "$$(strip $$($1_$2_HS_SRCS) $$($1_$2_HS_BOOT_SRCS))" ""
ifneq "$$(NO_STAGE$3_DEPS)" "YES"
include $$($1_$2_depfile_haskell)
endif
endif
include $$($1_$2_depfile_c_asm)
else
ifeq "$$(DEBUG)" "YES"
$$(warning not building dependencies in $1)
endif
endif

endef

