#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.50 2001/06/06 10:35:39 rrt Exp $
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

TOP=..
DoingRTS=YES
include $(TOP)/mk/boilerplate.mk

SplitObjs=NO

WAYS=$(GhcLibWays)

SRCS_RTS_C  = $(wildcard *.c) $(wildcard hooks/*.c) $(filter-out parallel/SysMan.c,$(wildcard parallel/*.c))
SRCS_RTS_S  = $(wildcard *.S)
SRCS_RTS_HC = $(wildcard *.hc) $(wildcard parallel/*.hc)

ifneq "$(DLLized)" "YES"
SRCS_RTS_C  := $(filter-out RtsDllMain.c, $(SRCS_RTS_C))
else
SRCS_RTS_C  := $(filter-out Main.c, $(SRCS_RTS_C))
endif

LIBOBJS = $(patsubst %.c,%.$(way_)o,$(SRCS_RTS_C)) \
          $(patsubst %.hc,%.$(way_)o,$(SRCS_RTS_HC)) \
          $(patsubst %.S,%.$(way_)o,$(SRCS_RTS_S))

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

SRC_CC_OPTS +=  $(GhcRtsCcOpts)  $(WARNING_OPTS) -DCOMPILING_RTS

SRC_HC_OPTS += -I../includes -I. -Iparallel $(GhcRtsHcOpts) -DCOMPILING_RTS

ifneq "$(DLLized)" "YES"
SRC_HC_OPTS += -static
endif
# SRC_HC_OPTS += -fPIC

ifeq "$(way)" "mp"
SRC_HC_OPTS += -I$$PVM_ROOT/include
endif

#-----------------------------------------------------------------------------
# Include the Front panel code?

SRCS_RTS_C :=  $(filter-out Vis%.c, $(SRCS_RTS_C))

# we need GTK+ for the front panel
ifneq "$(GTK_CONFIG)" ""
ifeq "$(GhcRtsWithFrontPanel)" "YES"
SRC_HC_OPTS 		+= `$(GTK_CONFIG) --cflags` -optc-DRTS_GTK_FRONTPANEL
SRCS_RTS_C 		:= $(SRCS_RTS_C) $(wildcard Vis*.c)
VisCallbacks_CC_OPTS 	+= -Wno-unused
endif # GhcRtsWithFrontPanel
endif # GTK_CONFIG

#-----------------------------------------------------------------------------

C_SRCS = $(SRCS_RTS_C) $(SRCS_RTS_HC) $(SRCS_RTS_S)

SRC_MKDEPENDC_OPTS += -I. -I../includes

#-----------------------------------------------------------------------------
#
# Compiling the individual files
#
# Rules for building various types of objects from C files,
# override the default suffix rule here, as we want to use
# ../driver/ghc (a better C compiler :-) to compile the
# different RTS pieces
#

ifeq "$(BootingFromHc)" "YES"
# use the normal $(CC) when booting from .hc files
SRC_CC_OPTS += $(HC_OPTS)
else
# otherwise, use $(GHC_INPLACE) as the C compiler.
CC=$(GHC_INPLACE)
CC_OPTS := $(addprefix -optc, $(CC_OPTS)) $(HC_OPTS)
endif

HC=$(GHC_INPLACE)

# prevent this value from leaking into the GMP makefile
unexport CC

# -----------------------------------------------------------------------------
#
#  Building DLLs is only supported on mingw32 at the moment.
#
HSLIB             = rts

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
	cd gmp && ./configure --enable-shared=no --target=$(HOSTPLATFORM)
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

clean distclean maintainer-clean ::
	$(MAKE) -C gmp MAKEFLAGS= $@

ifeq "$(way)" ""
INSTALL_LIBS += gmp/libgmp.a
endif

gmp/libgmp.a ::
	$(MAKE) -C gmp MAKEFLAGS=
	@$(CP) gmp/.libs/libgmp.a gmp
endif

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
