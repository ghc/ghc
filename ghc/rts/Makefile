#-----------------------------------------------------------------------------
#
#  This is the Makefile for the runtime-system stuff.
#  This stuff is written in C (and cannot be written in Haskell).
#
#  .c  files are vanilla C,
#  .hc files are "Haskellized-C", compiled using the C compiler and
#      (possibly) the assembly-mangler.  The GHC driver script
#      knows how to compile this stuff.
#
#  Other sorta independent, compile-once subdirs are:
#	gmp		-- GNU multi-precision library (for Integer)

#-----------------------------------------------------------------------------
# Preamble

TOP=..

# Set UseGhcForCc: this causes the fptools build system to use a different
# set of suffix rules for compiling C code, using $(HC) rather than $(CC)
# and prepending "-optc" to $(CC_OPTS).  NB. must be done before including
# boilerplate.mk below.
UseGhcForCc = YES

include $(TOP)/mk/boilerplate.mk

HC=$(GHC_INPLACE)

WAYS=$(GhcLibWays)

PACKAGE = rts

# Tells the build system not to add various Haskellish options to $(SRC_HC_OPTS)
NON_HS_PACKAGE = YES

# grab sources from these subdirectories
ALL_DIRS = hooks parallel

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
ALL_DIRS += win32
EXCLUDED_SRCS += Itimer.c Select.c Signals.c
endif

ifneq "$(DLLized)" "YES"
EXCLUDED_SRCS += RtsDllMain.c
else
EXCLUDED_SRCS += Main.c
endif

EXCLUDED_SRCS += parallel/SysMan.c

# The build system doesn't give us these
HC_SRCS = $(filter %.hc, $(SRCS))
HC_OBJS = $(patsubst %.hc,%.$(way_)o, $(HC_SRCS))

CLEAN_FILES += $(HC_OBJS)

# Override the default $(LIBOBJS) (defaults to $(HS_OBJS))
LIBOBJS = $(C_OBJS) $(HC_OBJS)

SplitObjs=NO

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
WARNING_OPTS += -Wbad-function-cast
#WARNING_OPTS += -Wcast-align
#WARNING_OPTS += -Wnested-externs
#WARNING_OPTS += -Wshadow
#WARNING_OPTS += -Wcast-qual
#WARNING_OPTS += -Wno-unused 
#WARNING_OPTS += -Wredundant-decls 
#WARNING_OPTS += -Wconversion

STANDARD_OPTS += -I../includes -I. -Iparallel
# COMPILING_RTS is only used when building Win32 DLL support.
STANDARD_OPTS += -DCOMPILING_RTS

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
STANDARD_OPTS += -Iwin32
endif

# HC_OPTS is included in both .c and .hc compilations, whereas CC_OPTS is
# only included in .c compilations.  HC_OPTS included the WAY_* opts, which
# must be included in both types of compilations.

SRC_CC_OPTS += $(WARNING_OPTS)
SRC_CC_OPTS += $(STANDARD_OPTS)

SRC_CC_OPTS += $(GhcRtsCcOpts)
SRC_HC_OPTS += $(GhcRtsHcOpts)

ifneq "$(DLLized)" "YES"
SRC_HC_OPTS += -static
endif
# SRC_HC_OPTS += -fPIC

ifeq "$(way)" "mp"
SRC_HC_OPTS += -I$$PVM_ROOT/include
endif

# You get 'threads support' in the normal
# and profiling ways.
ifeq "$(GhcRtsThreaded)" "YES"
ifeq "$(way)" ""
SRC_CC_OPTS += -DTHREADED_RTS
SRC_HC_OPTS += -optc-DTHREADED_RTS
PACKAGE_CPP_OPTS += -DTHREADED_RTS
endif
ifeq "$(way)" "p"
SRC_CC_OPTS += -DTHREADED_RTS
SRC_HC_OPTS += -optc-DTHREADED_RTS
PACKAGE_CPP_OPTS += -DTHREADED_RTS
endif
endif

# If -DDEBUG is in effect, adjust package conf accordingly..
ifneq "$(strip $(filter -optc-DDEBUG,$(GhcRtsHcOpts)))" ""
PACKAGE_CPP_OPTS += -DDEBUG
endif

ifeq "$(HaveLibMingwEx)" "YES"
PACKAGE_CPP_OPTS += -DHAVE_LIBMINGWEX
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
# make depend setup

MKDEPENDC_SRCS     = $(C_SRCS) $(HC_SRCS)
SRC_MKDEPENDC_OPTS += -I. -I../includes

# -----------------------------------------------------------------------------
# The auto-generated apply code

AUTO_APPLY = AutoApply.hc

ifneq "$(BootingFromHc)" "YES"
$(AUTO_APPLY): $(GHC_GENAPPLY)
	@$(RM) $@
	$(GHC_GENAPPLY) >$@
endif

EXTRA_SRCS += $(AUTO_APPLY)

CLEAN_FILES += $(AUTO_APPLY)

# -----------------------------------------------------------------------------
#
#  Building DLLs is only supported on mingw32 at the moment.
#
ifeq "$(DLLized)" "YES"
SRC_BLD_DLL_OPTS += -lHS_imp_stub -lgmp_imp

# It's not included in the DLL, but we need to compile it up separately.
all :: Main.dll_o

# Need an import library containing the symbols the RTS uses from the Prelude.
# So, to avoid bootstrapping trouble, we build one containing just the syms
# we need. Weirdly named to avoid clashing later on when compiling the contents
# of ghc/lib/..
#
# Note: if you do change the name of the Prelude DLL, the "--dllname <nm>.dll"
# below will need to be updated as well.

$(DLL_PEN)/HSrts$(_way).dll :: libHS_imp_stub.a

libHS_imp_stub.a :
	dlltool --output-lib libHS_imp_stub.a --def HSprel.def --dllname HSstd.dll

endif

# -----------------------------------------------------------------------------
# Compile GMP only if we don't have it already
#
# We use GMP's own configuration stuff, because it's all rather hairy
# and not worth re-implementing in our Makefile framework.

ifneq "$(HaveLibGmp)" "YES"
ifneq "$(HaveFrameworkHaskellSupport)" "YES"
boot ::
	cd gmp && CC=$(WhatGccIsCalled) ./configure --enable-shared=no \
		--host=`echo $(HOSTPLATFORM) | sed 's/i[567]86/i486/g'`

# Slight cheatage here to pass host as target, but x-compilation isn't supported by ghc.

all :: gmp/libgmp.a

ifeq "$(DLLized)" "YES"
all :: $(DLL_PEN)/gmp.dll

$(DLL_PEN)/gmp.dll:
	$(MAKE) -C gmp gmp.dll
	$(MV) gmp/gmp.dll $(DLL_PEN)
endif

install :: gmp/libgmp.a

ifeq "$(way)" ""
clean distclean maintainer-clean ::
	-$(MAKE) -C gmp MAKEFLAGS= $@

INSTALL_LIBS += gmp/libgmp.a
endif
endif

gmp/libgmp.a ::
	$(MAKE) -C gmp MAKEFLAGS=
	@$(CP) gmp/.libs/libgmp.a gmp
	@$(RANLIB) gmp/libgmp.a
endif

CLEAN_FILES += gmp/libgmp.a

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
#
# Files to install
#
# Just libHSrts is installed uniformly across ways
#
INSTALL_LIBS += $(LIBRARY)
ifeq "$(DLLized)" "YES"
INSTALL_PROGS += $(DLL_NAME) gmp/gmp.dll
INSTALL_LIBS += $(patsubst %.a,%_imp.a,$(LIBARY))
INSTALL_LIBS += gmp/libgmp_imp.a Main.dll_o
endif

include $(TOP)/mk/target.mk
