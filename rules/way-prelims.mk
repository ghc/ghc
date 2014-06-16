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

ifneq "$(findstring dyn,$1)" ""
#  If the way includes "dyn" then it's a dynamic lib way. We mangle the
#  way name to remove "dyn" (or "_dyn") and we change the suffix to
#  include the versioned dynamic lib extension (eg .so or .dynlib).
#  For example: thr_debug_dyn_libsuf="_thr_debug-ghc6.11.20090426.so"
$1_libsuf  = $$($(subst dyn,,$(subst _dyn,,$1))__way)-ghc$(ProjectVersion)$(soext)
else
$1_libsuf  = $$($1__way).a
endif
endef
