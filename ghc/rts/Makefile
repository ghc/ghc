#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.59 2002/01/30 16:27:34 simonmar Exp $
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
ifneq "$(BootingFromHc)" "YES"
UseGhcForCc = YES
endif

include $(TOP)/mk/boilerplate.mk

HC=$(GHC_INPLACE)

PACKAGE = rts
WAYS=$(GhcLibWays)

# grab sources from these subdirectories
ALL_DIRS = hooks parallel

ifneq "$(DLLized)" "YES"
EXCLUDED_SRCS += RtsDllMain.c
else
EXCLUDED_SRCS += Main.c
endif

EXCLUDED_SRCS += parallel/SysMan.c

# The build system doesn't give us these
HC_SRCS = $(filter %.hc, $(SRCS))
HC_OBJS = $(patsubst %.hc,%.o, $(HC_SRCS))

# Override the default $(LIBOBJS) (the default provides for building Haskell libs)
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
WARNING_OPTS += -Wcast-align
#WARNING_OPTS += -Wnested-externs
#WARNING_OPTS += -Wshadow
#WARNING_OPTS += -Wcast-qual
#WARNING_OPTS += -Wno-unused 
#WARNING_OPTS += -Wredundant-decls 
#WARNING_OPTS += -Wconversion

SRC_CC_OPTS += $(WARNING_OPTS)

SRC_CC_OPTS += -I../includes -I. -Iparallel 
SRC_HC_OPTS += -I../includes -I. -Iparallel 

SRC_CC_OPTS += $(GhcRtsCcOpts) -DCOMPILING_RTS
SRC_HC_OPTS += $(GhcRtsHcOpts) -DCOMPILING_RTS

ifneq "$(DLLized)" "YES"
SRC_HC_OPTS += -static
endif
# SRC_HC_OPTS += -fPIC

ifeq "$(way)" "mp"
SRC_HC_OPTS += -I$$PVM_ROOT/include
endif

ifeq "$(BootingFromHc)" "YES"
# use the normal $(CC) when booting from .hc files
SRC_CC_OPTS += $(HC_OPTS)
endif

#-----------------------------------------------------------------------------
# Include the Front panel code?

# we need GTK+ for the front panel
ifneq "$(GTK_CONFIG)" ""
ifeq "$(GhcRtsWithFrontPanel)" "YES"
SRC_HC_OPTS 		+= `$(GTK_CONFIG) --cflags` -optc-DRTS_GTK_FRONTPANEL
VisCallbacks_CC_OPTS 	+= -Wno-unused
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
ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
boot ::
	cd gmp && ./configure --enable-shared=no \
		--host=`echo $(HOSTPLATFORM) | sed 's/i[567]86/i486/g'`
else
# Pass --target to configure of GMP, so that building for mingwin under
# cygwin works properly (when the host is not the same as the target)
boot ::
	cd gmp && CC=$(WhatGccIsCalled) ./configure --enable-shared=no --target=$(HOSTPLATFORM)
endif

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
