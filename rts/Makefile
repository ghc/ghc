#-----------------------------------------------------------------------------
#
#  This is the Makefile for the runtime-system stuff.
#  This stuff is written in C (and cannot be written in Haskell).
#
#  .c  files are vanilla C,
#  .hc files are "Haskellized-C", compiled using the C compiler and
#      (possibly) the assembly-mangler.  The GHC driver script
#      knows how to compile this stuff.

#-----------------------------------------------------------------------------
# Preamble

TOP=..

# Set UseGhcForCc: this causes the fptools build system to use a different
# set of suffix rules for compiling C code, using $(HC) rather than $(CC)
# and prepending "-optc" to $(CC_OPTS).  NB. must be done before including
# boilerplate.mk below.
UseGhcForCc = YES

include $(TOP)/mk/boilerplate.mk

PACKAGE = rts

HC=$(GHC_INPLACE)

# -----------------------------------------------------------------------------
# RTS ways

WAYS=$(GhcLibWays) $(GhcRTSWays)

ifneq "$(findstring debug, $(way))" ""
GhcRtsHcOpts=
GhcRtsCcOpts=-g -O0
endif

ifeq "$(BeConservative)" "YES"
GhcRtsCcOpts += -DBE_CONSERVATIVE
endif

# -----------------------------------------------------------------------------

# There's nothing for Haddock here...
override HADDOCK_DOCS = NO

# -----------------------------------------------------------------------------

# Tells the build system not to add various Haskellish options to $(SRC_HC_OPTS)
NON_HS_PACKAGE = YES

# grab sources from these subdirectories
ALL_DIRS = hooks parallel sm

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
ALL_DIRS += win32
else
ALL_DIRS += posix
endif

ifneq "$(findstring dyn, $(way))" ""
DYNAMIC_RTS=YES
else
DYNAMIC_RTS=NO
endif

ifeq "$(DYNAMIC_RTS) $(HOSTPLATFORM)" "YES i386-unknown-mingw32"
EXCLUDED_SRCS += Main.c
# It's not included in the DLL, but we need to compile it up separately.

all :: Main.$(way_)o
INSTALL_LIBS += Main.$(way_)o
else
EXCLUDED_SRCS += RtsDllMain.c
endif

# This file ends up being empty unless we're building for a powerpc
# or darwin system, and it is reported that Solaris ld chokes on it when
# building HSrts.o.
ifeq "$(findstring $(TargetArch_CPP), powerpc powerpc64)" ""
ifeq "$(findstring $(TargetOS_CPP), darwin)" ""
EXCLUDED_SRCS += AdjustorAsm.S
endif
endif

EXCLUDED_SRCS += parallel/SysMan.c

# The build system doesn't give us these
CMM_SRCS = $(filter-out AutoApply%.cmm, $(wildcard *.cmm)) $(EXTRA_CMM_SRCS)
CMM_OBJS = $(patsubst %.cmm,%.$(way_)o, $(CMM_SRCS))

CLEAN_FILES += $(CMM_OBJS)

# Override the default $(LIBOBJS) (defaults to $(HS_OBJS))
LIBOBJS = $(C_OBJS) $(CMM_OBJS)

SplitObjs=NO

H_FILES = $(wildcard ../includes/*.h) $(wildcard *.h)

#-----------------------------------------------------------------------------
# Flags for compiling RTS .c and .hc files

# gcc provides lots of useful warnings if you ask it.
# This is a pretty good list to start with - use a # to comment out
# any you don't like.
WARNING_OPTS += -Wall
WARNING_OPTS += -W
WARNING_OPTS += -Wstrict-prototypes 
WARNING_OPTS += -Wmissing-prototypes 
WARNING_OPTS += -Wmissing-declarations
WARNING_OPTS += -Winline
WARNING_OPTS += -Waggregate-return
#WARNING_OPTS += -Wpointer-arith
#WARNING_OPTS += -Wbad-function-cast
#WARNING_OPTS += -Wcast-align
#WARNING_OPTS += -Wnested-externs
#WARNING_OPTS += -Wshadow
#WARNING_OPTS += -Wcast-qual
#WARNING_OPTS += -Wno-unused 
#WARNING_OPTS += -Wredundant-decls 
#WARNING_OPTS += -Wconversion

STANDARD_OPTS += -I../includes -I. -Iparallel -Ism
# COMPILING_RTS is only used when building Win32 DLL support.
STANDARD_OPTS += -DCOMPILING_RTS

# HC_OPTS is included in both .c and .cmm compilations, whereas CC_OPTS is
# only included in .c compilations.  HC_OPTS included the WAY_* opts, which
# must be included in both types of compilations.

SRC_CC_OPTS += $(WARNING_OPTS)
SRC_CC_OPTS += $(STANDARD_OPTS)

SRC_CC_OPTS += $(GhcRtsCcOpts)
SRC_HC_OPTS += $(GhcRtsHcOpts) $(STANDARD_OPTS) -package-name rts

ifneq "$(GhcWithSMP)" "YES"
SRC_CC_OPTS += -DNOSMP
SRC_HC_OPTS += -optc-DNOSMP
endif

ifneq "$(DYNAMIC_RTS)" "YES"
SRC_HC_OPTS += -static
else
LIB_LD_OPTS += -ignore-package base -ignore-package rts
ifeq "$(DYNAMIC_RTS) $(HOSTPLATFORM)" "YES i386-unknown-mingw32"
BASE_VERSION=$(strip $(shell grep version: $(TOP)/libraries/base/base.cabal | cut -f2 -d:))
BASE_NAME=HSbase-$(BASE_VERSION)-ghc$(ProjectVersion)$(soext)
BASE_DIST_LIB=$(TOP)/libraries/base/dist/build
BASE_IMPORT_LIBRARY=$(BASE_DIST_LIB)/lib$(BASE_NAME).a
LIB_DEPS=$(BASE_IMPORT_LIBRARY)
LIB_LD_OPTS += -L$(BASE_DIST_LIB) -l$(BASE_NAME)
# We extract a good bit of information out of the rts package.conf by going via ghc-pkg
LIB_LD_OPTS += $(foreach lib,$(shell $(GHC_PKG_INPLACE) field rts extra-libraries | sed -e s/extra-libraries://),"-l$(lib)")
LIB_LD_OPTS += $(foreach libdir,$(shell $(GHC_PKG_INPLACE) field rts library-dirs | sed -e s/library-dirs://),"-L$(libdir)")
endif
endif

ifeq "$(Windows)" "YES"
# On Windows GHC does its own commandline parsing, so we need extra
# doublequote protection. Sigh.
DQ = \\\"
else
DQ = \"
endif

# If Main.c is built with optimisation then the SEH exception stuff on
# Windows gets confused.
# This has to be in HC rather than CC opts, as otherwise there's a
# -optc-O2 that comes after it.
Main_HC_OPTS += -optc-O0

RtsMessages_CC_OPTS += -DProjectVersion=$(DQ)$(ProjectVersion)$(DQ)
RtsUtils_CC_OPTS += -DProjectVersion=$(DQ)$(ProjectVersion)$(DQ)
RtsUtils_CC_OPTS += -DRtsWay=$(DQ)rts$(_way)$(DQ)
RtsUtils_CC_OPTS += -DHostPlatform=$(DQ)$(HOSTPLATFORM)$(DQ)
RtsUtils_CC_OPTS += -DBuildPlatform=$(DQ)$(BUILDPLATFORM)$(DQ)
RtsUtils_CC_OPTS += -DTargetPlatform=$(DQ)$(TARGETPLATFORM)$(DQ)
RtsUtils_CC_OPTS += -DGhcUnregisterised=$(DQ)$(GhcUnregisterised)$(DQ)
RtsUtils_CC_OPTS += -DGhcEnableTablesNextToCode=$(DQ)$(GhcEnableTablesNextToCode)$(DQ)

StgCRun_CC_OPTS += -w
Typeable_CC_OPTS += -w
RetainerProfile_CC_OPTS += -w
sm/Compact_CC_OPTS += -w
# On Windows:
win32/ConsoleHandler_CC_OPTS += -w
win32/ThrIOManager_CC_OPTS += -w
win32/Ticker_CC_OPTS += -w
Threads_CC_OPTS += -w
Capability_CC_OPTS += -w
Schedule_CC_OPTS += -w
# The above warning supression flags are a temporary kludge.
# While working on this module you are encouraged to remove it and fix
# any warnings in the module. See
#     http://hackage.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
# for details

ifeq "$(way)" "mp"
SRC_HC_OPTS += -I$$PVM_ROOT/include
endif

# If -DDEBUG is in effect, adjust package conf accordingly..
ifneq "$(strip $(filter -optc-DDEBUG,$(GhcRtsHcOpts)))" ""
PACKAGE_CPP_OPTS += -DDEBUG
endif

ifeq "$(HaveLibMingwEx)" "YES"
PACKAGE_CPP_OPTS += -DHAVE_LIBMINGWEX
endif

ifeq "$(GhciWithDebugger)" "YES"
STANDARD_OPTS += -DDEBUGGER
endif

ifeq "$(DotnetSupport)" "YES"

# 
# Would like to just use SUBDIRS here, but need to
# descend into dotnet/ earlier than that.
#
all ::
	$(MAKE) -C dotnet all

# But use SUBDIRS for other recursive targets.
SUBDIRS += dotnet

LIBOBJS += dotnet/Invoke.o
endif

# Suppress uninitialized variable warnings for GC.c
GC_CC_OPTS 	+= -Wno-uninitialized

#-----------------------------------------------------------------------------
# Include the Front panel code?

# we need GTK+ for the front panel
ifneq "$(GTK_CONFIG)" ""
ifeq "$(GhcRtsWithFrontPanel)" "YES"
SRC_HC_OPTS 		+= `$(GTK_CONFIG) --cflags` -optc-DRTS_GTK_FRONTPANEL
VisCallbacks_CC_OPTS 	+= -Wno-unused
SRC_MKDEPENDC_OPTS	+= `$(GTK_CONFIG) --cflags`
else # GhcRtsWithFrontPanel
EXCLUDED_SRCS		+= $(wildcard Vis*.c)
endif
else # GTK_CONFIG
EXCLUDED_SRCS		+= $(wildcard Vis*.c)
endif

#-----------------------------------------------------------------------------
# Add PAPI library if needed

ifeq "$(GhcRtsWithPapi)" "YES"
SRC_HC_OPTS		+= -optc-DUSE_PAPI
PACKAGE_CPP_OPTS	+= -DUSE_PAPI
endif

#-----------------------------------------------------------------------------
# make depend setup

SRC_MKDEPENDC_OPTS += -I. -I../includes

# Hack: we define every way-related option here, so that we get (hopefully)
# a superset of the dependencies.  To do this properly, we should generate
# a different set of dependencies for each way.  Further hack: PROFILING and
# TICKY_TICKY can't be used together, so we omit TICKY_TICKY for now.
SRC_MKDEPENDC_OPTS += -DPROFILING -DTHREADED_RTS -DDEBUG

# -----------------------------------------------------------------------------
# The auto-generated apply code

# We want a slightly different version for the unregisterised way, so we make
# AutoApply on a per-way basis (eg. AutoApply_p.cmm).

ifneq "$(DOING_BIN_DIST)" "YES"

AUTO_APPLY_CMM = AutoApply$(_way).cmm

ifneq "$(BootingFromHc)" "YES"
$(AUTO_APPLY_CMM): $(GHC_GENAPPLY)
	@$(RM) $@
	$(GENAPPLY) $(if $(filter $(way), u debug_u), -u) >$@
endif

EXTRA_CMM_SRCS += $(AUTO_APPLY_CMM)

CLEAN_FILES += $(AUTO_APPLY_CMM)

endif

# -----------------------------------------------------------------------------
# Compile GMP only if we don't have it already
#
# We use GMP's own configuration stuff, because it's all rather hairy
# and not worth re-implementing in our Makefile framework.

CLEAN_FILES += gmp/libgmp.a

# Need to get the GMP vars in through CPP to package.conf.in, and put
# quotes around each element.

empty =
space = $(empty) $(empty)
comma = ,
PACKAGE_CPP_OPTS += -DGMP_INCLUDE_DIRS='$(subst $(space),$(comma),$(patsubst %,"%",$(strip $(GMP_INCLUDE_DIRS))))'
PACKAGE_CPP_OPTS += -DGMP_LIB_DIRS='$(subst $(space),$(comma),$(patsubst %,"%",$(strip $(GMP_LIB_DIRS))))'

ifneq "$(GMP_INCLUDE_DIRS)" ""
SRC_HC_OPTS += -I$(GMP_INCLUDE_DIRS)
SRC_CC_OPTS += -I$(GMP_INCLUDE_DIRS)
SRC_HSC2HS_OPTS += -I$(GMP_INCLUDE_DIRS)
endif
ifneq "$(GMP_LIB_DIRS)" ""
SRC_LD_OPTS += -L$(GMP_LIB_DIRS)
endif

SRC_CC_OPTS += -I../gmp/gmpbuild
SRC_HC_OPTS += -I../gmp/gmpbuild
SRC_HSC2HS_OPTS += -I../gmp/gmpbuild
SRC_LD_OPTS += -L../gmp/gmpbuild

#-----------------------------------------------------------------------------
#
# Building the GUM SysMan
#

ifeq "$(way)" "mp"
all :: parallel/SysMan

ifdef solaris2_TARGET_OS
__socket_libs = -lsocket -lnsl
else
__socket_libs =
endif

parallel/SysMan : parallel/SysMan.mp_o parallel/LLComms.mp_o RtsUtils.mp_o RtsFlags.mp_o
	$(RM) $@
	gcc -o $@ parallel/SysMan.mp_o parallel/LLComms.mp_o -L$$PVM_ROOT/lib/$$PVM_ARCH -lgpvm3 -lpvm3 $(__socket_libs)

CLEAN_FILES  += parallel/SysMan.mp_o parallel/SysMan
INSTALL_LIBEXECS += parallel/SysMan
endif

#-----------------------------------------------------------------------------
# Compiling the cmm files

# ToDo: should we really include Rts.h here?  Required for GNU_ATTRIBUTE().
SRC_HC_OPTS += -I. -\#include HCIncludes.h

ifeq "$(Windows)" "YES"
PrimOps_HC_OPTS += -\#include '<windows.h>' -\#include win32/AsyncIO.h
else
PrimOps_HC_OPTS += -\#include posix/Itimer.h
endif

# Otherwise the stack-smash handler gets triggered.
ifeq "$(TargetOS_CPP)" "openbsd"
SRC_HC_OPTS += -optc-fno-stack-protector
endif

# -O3 helps unroll some loops (especially in copy() with a constant argument).
sm/Evac_HC_OPTS += -optc-funroll-loops

# Without this, thread_obj will not be inlined (at least on x86 with GCC 4.1.0)
sm/Compact_HC_OPTS += -optc-finline-limit=2500

# -fno-strict-aliasing is required for the runtime, because we often
# use a variety of types to represent closure pointers (StgPtr,
# StgClosure, StgMVar, etc.), and without -fno-strict-aliasing gcc is
# allowed to assume that these pointers do not alias.  eg. without
# this flag we get problems in GC.c:copy() with gcc 3.4.3, the
# upd_evacee() assigments get moved before the object copy.
SRC_CC_OPTS += -fno-strict-aliasing

# We *want* type-checking of hand-written cmm.
SRC_HC_OPTS += -dcmm-lint 

ifneq "$(BootingFromHc)" "YES"
# .cmm files depend on all the .h files, to a first approximation.
%.$(way_)o : %.cmm $(H_FILES)
	$(HC_PRE_OPTS)
	$(HC) $(HC_OPTS) -c $< -o $@
	$(HC_POST_OPTS)

%.$(way_)hc : %.cmm $(H_FILES)
	$(HC) $(HC_OPTS) -C $< -o $@

%.$(way_)s : %.cmm $(H_FILES)
	$(HC) $(HC_OPTS) -S $< -o $@
endif

#-----------------------------------------------------------------------------
#
# Files to install
#
# Just libHSrts is installed uniformly across ways
#

include $(TOP)/mk/target.mk

ifeq "$(DYNAMIC_RTS) $(HOSTPLATFORM)" "YES i386-unknown-mingw32"
$(BASE_IMPORT_LIBRARY): $(LIBRARY).a
	$(MAKE) -C ../libraries/ make.library.base
# just for the timestamps
	touch $(BASE_IMPORT_LIBRARY)
endif

#-----------------------------------------------------------------------------
#
# binary-dist

binary-dist:
	$(INSTALL_DIR)                         $(BIN_DIST_DIR)/rts
	$(INSTALL_DIR)                         $(BIN_DIST_DIR)/rts/gmp
	$(INSTALL_DATA)    Makefile            $(BIN_DIST_DIR)/rts/
	$(INSTALL_DATA)    package.conf.in     $(BIN_DIST_DIR)/rts/
ifneq "$(INSTALL_LIBS)" ""
	$(INSTALL_DATA)    $(INSTALL_LIBS)     $(BIN_DIST_DIR)/rts/
endif
ifneq "$(INSTALL_LIBEXECS)" ""
	$(INSTALL_PROGRAM) $(INSTALL_LIBEXECS) $(BIN_DIST_DIR)/rts/
endif
