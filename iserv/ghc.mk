# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

iserv_USES_CABAL = YES
iserv_PACKAGE = iserv-bin
iserv_EXECUTABLE = iserv

ifeq "$(GhcDebugged)" "YES"
iserv_stage2_MORE_HC_OPTS += -debug
iserv_stage2_p_MORE_HC_OPTS += -debug
iserv_stage2_dyn_MORE_HC_OPTS += -debug
endif

iserv_stage2_MORE_HC_OPTS += -threaded
iserv_stage2_p_MORE_HC_OPTS += -threaded
iserv_stage2_dyn_MORE_HC_OPTS += -threaded

# Override the default way, because we want a specific version of this
# program for each way.  Note that it's important to do this even for
# the vanilla version, otherwise we get a dynamic executable when
# DYNAMIC_GHC_PROGRAMS=YES.
iserv_stage2_PROGRAM_WAY = v
iserv_stage2_p_PROGRAM_WAY = p
iserv_stage2_dyn_PROGRAM_WAY = dyn

iserv_stage2_PROGNAME = ghc-iserv
iserv_stage2_p_PROGNAME = ghc-iserv-prof
iserv_stage2_dyn_PROGNAME = ghc-iserv-dyn

iserv_stage2_MORE_HC_OPTS += -no-hs-main
iserv_stage2_p_MORE_HC_OPTS += -no-hs-main
iserv_stage2_dyn_MORE_HC_OPTS += -no-hs-main

iserv_stage2_INSTALL = YES
iserv_stage2_p_INSTALL = YES
iserv_stage2_dyn_INSTALL = YES

# Install in $(libexec), not in $(bindir)
iserv_stage2_TOPDIR = YES
iserv_stage2_p_TOPDIR = YES
iserv_stage2_dyn_TOPDIR = YES

iserv_stage2_INSTALL_INPLACE = YES
iserv_stage2_p_INSTALL_INPLACE = YES
iserv_stage2_dyn_INSTALL_INPLACE = YES

$(eval $(call build-prog,iserv,stage2,1))

ifeq "$(CLEANING)" "YES"

NEED_iserv_p = YES
NEED_iserv_dyn = YES

else

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

ifeq "$(NEED_iserv_p)" "YES"
$(eval $(call build-prog,iserv,stage2_p,1))
endif

ifeq "$(NEED_iserv_dyn)" "YES"
$(eval $(call build-prog,iserv,stage2_dyn,1))
endif

all_ghc_stage2 : $(iserv-stage2_INPLACE)
all_ghc_stage2 : $(iserv-stage2_p_INPLACE)
all_ghc_stage2 : $(iserv-stage2_dyn_INPLACE)
