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

define way-prelims # $1 = way
ifeq "$1" "v"
$1__way  =
$1_way_  =
else
$1__way  = _$1
$1_way_  = $1_
endif
$1_osuf   = $$($1_way_)o
$1_hisuf  = $$($1_way_)hi
$1_hcsuf  = $$($1_way_)hc
endef
