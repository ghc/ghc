#-----------------------------------------------------------------------------
#

TOP=..
CURRENT_DIR=ghc/driver
include $(TOP)/mk/boilerplate.mk

INSTALLING=0

DYN_LOADABLE_BITS = \
	ghc-asm.prl \
	ghc-iface.prl \
	ghc-consist.prl \
	ghc-split.prl

SCRIPT_PROG = ghc-$(GhcProjectVersion)
SCRIPT_LINK = ghc
SCRIPT_OBJS = ghc.prl
 
INTERP=perl

#
# The driver needs to know the options and names for 
# all possible ways, so we magically generate the
# the make variable names for them here.
#

WAY_NAMES = $(foreach way,$(ALL_WAYS),WAY_$(way)_NAME)
WAY_OPTS  = $(foreach way,$(ALL_WAYS),WAY_$(way)_HC_OPTS)

ifeq "$(INSTALLING)" "1"
TOP_PWD := $(prefix)
else
TOP_PWD := $(FPTOOLS_TOP_ABS)
endif

SCRIPT_SUBST_VARS := \
  INSTALLING \
  ProjectName ProjectVersion ProjectVersionInt ProjectPatchLevel \
  CURRENT_DIR HOSTPLATFORM TARGETPLATFORM \
  GHC_LIB_DIR GHC_RUNTIME_DIR GHC_UTILS_DIR GHC_INCLUDE_DIR \
  GHC_OPT_HILEV_ASM GhcWithNativeCodeGen LeadingUnderscore\
  GHC_UNLIT GHC_HSCPP GHC_HSC GHC_SYSMAN \
  CP RM CONTEXT_DIFF \
  $(WAY_NAMES) $(WAY_OPTS)

#
# When creating a binary distribution, we prefix the driver script
# with a short msg about what variables need to be set to get the
# script going. 
#

ifeq "$(BIN_DIST)" "1"
SCRIPT_PREFIX_FILES=prefix.txt
else
SCRIPT_SUBST_VARS += libdir libexecdir datadir bindir TMPDIR TOP_PWD
endif

all :: $(DYN_LOADABLE_BITS)

#
# Install setup:
#   the driver goes in $(bindir), the perl script helpers
#   in $(libdir)
#
# ToDo: allow different install name for driver?
#
INSTALL_SCRIPTS += $(SCRIPT_PROG)
INSTALL_LIBS    += $(DYN_LOADABLE_BITS)

#
# Before really installing the driver, we have to
# reconfigure it such that the paths it refers to,
# point to the installed utils.
#
install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) INSTALLING=1 $(SCRIPT_PROG)

#
# depend setup: other directories need the driver script to compute
# their dependencies, so `depend' is simply an alias for `all' here.
depend :: all

#
# Clean up
#
CLEAN_FILES += $(SCRIPT_OBJS) $(SCRIPT_LINK) $(DYN_LOADABLE_BITS) ghc-[0-9].*

#
# Source distribution
#
SRC_DEST_FILES=$(patsubst %.prl,%.lprl,$(DYN_LOADABLE_BITS)) ghc.lprl ordering-passes test_mangler

include $(TOP)/mk/target.mk

# Hack to re-create the in-situ build tree driver script after 
# having installed it.
#
install ::
	@$(RM) $(SCRIPT_PROG)
	@$(MAKE) $(MFLAGS) BIN_DIST=0 $(SCRIPT_PROG)


#
# Option vars for the special ways (that the driver has special pleading for).
#
# ToDo: rename -DPROFILING to -D__SCC_PROFILING (or somesuch)
#              -DTICKY-TICKY TO __TICKY_TICKY__
#
#  (this is to make the naming consistent with other `standard' hscpp #defines )

# Way p:
WAY_p_NAME=profiling
WAY_p_HC_OPTS+=-fscc-profiling -DPROFILING -optc-DPROFILING

# Way t:
WAY_t_NAME+=ticky-ticky profiling
WAY_t_HC_OPTS=-fticky-ticky -DTICKY_TICKY -optc-DTICKY_TICKY

# Way `u':
WAY_u_NAME=unregisterized (using portable C only)
WAY_u_HC_OPTS=

# Way `mc': concurrent
WAY_mc_NAME=concurrent
WAY_mc_HC_OPTS+=-fstack-check -fconcurrent -D__CONCURRENT_HASKELL__ -optc-DCONCURRENT

# Way `mr': 
WAY_mr_NAME=profiled concurrent
WAY_mr_HC_OPTS+=-fstack-check -fconcurrent -fscc-profiling -D__CONCURRENT_HASKELL__ -DPROFILING -optc-DCONCURRENT -optc-DPROFILING

# Way `mt': 
WAY_mt_NAME=ticky-ticky concurrent
WAY_mt_HC_OPTS+=-fstack-check -fconcurrent -fticky-ticky -D__CONCURRENT_HASKELL__ -DTICKY-TICKY -optc-DCONCURRENT -optc-DTICKY_TICKY

# Way `mp': 
WAY_mp_NAME=parallel
WAY_mp_HC_OPTS+=-fstack-check -fconcurrent -D__PARALLEL_HASKELL__ -optc-DPAR -optc-DCONCURRENT

#
# Way `mg': 
#  Q: is passing -D__GRANSIM__ and -DGRAN to hscpp needed? No, just -D__GRANSIM__
WAY_mg_NAME=GranSim
WAY_mg_HC_OPTS+=-fstack-check -fconcurrent -fgransim -D__GRANSIM__ -D__CONCURRENT_HASKELL__ -optc-DCONCURRENT -optc-DGRAN

#
# Ways for different garbage collectors
#
WAY_2s_NAME=2-space GC
WAY_2s_HC_OPTS+=-optc-DGC2s

WAY_1s_NAME=1-space GC
WAY_1s_HC_OPTS+=-optc-DGC1s

WAY_du_NAME=dual-mode GC
WAY_du_HC_OPTS+=-optc-DGCdu
