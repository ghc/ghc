# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------

utils/iserv_USES_CABAL = YES
utils/iserv_PACKAGE = iserv
utils/iserv_EXECUTABLE = iserv

ifeq "$(GhcDebugged)" "YES"
utils/iserv_stage2_MORE_HC_OPTS += -debug
utils/iserv_stage2_p_MORE_HC_OPTS += -debug
utils/iserv_stage2_dyn_MORE_HC_OPTS += -debug
endif

ifeq "$(GhcThreaded)" "YES"
utils/iserv_stage2_MORE_HC_OPTS += -threaded
utils/iserv_stage2_p_MORE_HC_OPTS += -threaded
utils/iserv_stage2_dyn_MORE_HC_OPTS += -threaded
endif

# Add -Wl,--export-dynamic enables GHCi to load dynamic objects that
# refer to the RTS.  This is harmless if you don't use it (adds a bit
# of overhead to startup and increases the binary sizes) but if you
# need it there's no alternative.
# Don't do this on FreeBSD to work around #17962.
ifeq "$(TargetElf)" "YES"
ifeq "$(findstring $(TargetOS_CPP), solaris2 freebsd)" ""
# The Solaris linker does not support --export-dynamic option. It also
# does not need it since it exports all dynamic symbols by default
utils/iserv_stage2_MORE_HC_OPTS += -optl-Wl,--export-dynamic
utils/iserv_stage2_p_MORE_HC_OPTS += -optl-Wl,--export-dynamic
utils/iserv_stage2_dyn_MORE_HC_OPTS += -optl-Wl,--export-dynamic
endif
endif

# Override the default way, because we want a specific version of this
# program for each way.  Note that it's important to do this even for
# the vanilla version, otherwise we get a dynamic executable when
# DYNAMIC_GHC_PROGRAMS=YES.
utils/iserv_stage2_PROGRAM_WAY = v
utils/iserv_stage2_p_PROGRAM_WAY = p
utils/iserv_stage2_dyn_PROGRAM_WAY = dyn

utils/iserv_stage2_PROGNAME = ghc-iserv
utils/iserv_stage2_p_PROGNAME = ghc-iserv-prof
utils/iserv_stage2_dyn_PROGNAME = ghc-iserv-dyn

utils/iserv_stage2_MORE_HC_OPTS += -no-hs-main
utils/iserv_stage2_p_MORE_HC_OPTS += -no-hs-main
utils/iserv_stage2_dyn_MORE_HC_OPTS += -no-hs-main

utils/iserv_stage2_INSTALL = YES
utils/iserv_stage2_p_INSTALL = YES
utils/iserv_stage2_dyn_INSTALL = YES

# Install in $(libexec), not in $(bindir)
utils/iserv_stage2_TOPDIR = YES
utils/iserv_stage2_p_TOPDIR = YES
utils/iserv_stage2_dyn_TOPDIR = YES

utils/iserv_stage2_INSTALL_INPLACE = YES
utils/iserv_stage2_p_INSTALL_INPLACE = YES
utils/iserv_stage2_dyn_INSTALL_INPLACE = YES

ifeq "$(CLEANING)" "YES"

NEED_iserv = YES
NEED_iserv_p = YES
NEED_iserv_dyn = YES

else

ifneq "$(findstring v, $(GhcLibWays))" ""
NEED_iserv = YES
else
NEED_iserv = NO
endif

ifneq "$(findstring p, $(GhcLibWays))" ""
NEED_iserv_p = YES
else
NEED_iserv_p = NO
endif

ifneq "$(findstring dyn, $(GhcLibWays))" ""
NEED_iserv_dyn = YES
else
NEED_iserv_dyn = NO
endif
endif

ifeq "$(NEED_iserv)" "YES"
$(eval $(call build-prog,utils/iserv,stage2,1))
endif

ifeq "$(NEED_iserv_p)" "YES"
$(eval $(call build-prog,utils/iserv,stage2_p,1))
endif

ifeq "$(NEED_iserv_dyn)" "YES"
$(eval $(call build-prog,utils/iserv,stage2_dyn,1))
endif

all_ghc_stage2 : $(iserv-stage2_INPLACE)
all_ghc_stage2 : $(iserv-stage2_p_INPLACE)
all_ghc_stage2 : $(iserv-stage2_dyn_INPLACE)
