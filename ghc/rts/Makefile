#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.18 2000/03/14 01:30:21 sof Exp $
#
#  This is the Makefile for the runtime-system stuff.
#  This stuff is written in C (and cannot be written in Haskell).
#
#  We create two libraries.  One, libHSrts<tag>.a, is built separately
#  for each "way".  The other, libHSclib.a is built once: it is just
#  .lc files that end up the same no matter what, i.e. completely
#  ordinary C.

#  Other sorta independent, compile-once subdirs are:

#	gmp		-- GNU multi-precision library (for Integer)

#-----------------------------------------------------------------------------

TOP=..
DoingRTS=YES
include $(TOP)/mk/boilerplate.mk

WAYS=$(GhcLibWays)

SRCS_RTS_C  = $(wildcard *.c) $(wildcard hooks/*.c) $(filter-out parallel/SysMan.c,$(wildcard parallel/*.c))
SRCS_RTS_S  = $(wildcard *.S)
SRCS_RTS_HC = $(wildcard *.hc) $(wildcard parallel/*.hc)

ifneq "$(way)" "dll"
SRCS_RTS_C  := $(filter-out RtsDllMain.c, $(SRCS_RTS_C))
else
SRCS_RTS_C  := $(filter-out Main.c, $(SRCS_RTS_C))
endif

#-----------------------------------------------------------------------------
# creating and installing libHSrts.a (in its many flavors)
#
LIBRARY = libHSrts$(_way).a
LIBOBJS = $(patsubst %.c,%.$(way_)o,$(SRCS_RTS_C)) \
          $(patsubst %.hc,%.$(way_)o,$(SRCS_RTS_HC)) \
          $(patsubst %.S,%.$(way_)o,$(SRCS_RTS_S))

# gcc provides lots of useful warnings if you ask it.
# This is a pretty good list to start with - use a # to comment out
# any you don't like.
WARNING_OPTS += -optc-Wall 
WARNING_OPTS += -optc-W
WARNING_OPTS += -optc-Wstrict-prototypes 
WARNING_OPTS += -optc-Wmissing-prototypes 
WARNING_OPTS += -optc-Wmissing-declarations
WARNING_OPTS += -optc-Winline
WARNING_OPTS += -optc-Waggregate-return
WARNING_OPTS += -optc-Wpointer-arith
WARNING_OPTS += -optc-Wbad-function-cast
#WARNING_OPTS += -optc-Wcast-align
#WARNING_OPTS += -optc-Wnested-externs
#WARNING_OPTS += -optc-Wshadow
#WARNING_OPTS += -optc-Wcast-qual
#WARNING_OPTS += -optc-Wno-unused 
#WARNING_OPTS += -optc-Wredundant-decls 
#WARNING_OPTS += -optc-Wconversion

SRC_HC_OPTS += -I../includes -I. -Iparallel $(WARNING_OPTS) $(GhcRtsHcOpts) -optc-DCOMPILING_RTS
SRC_CC_OPTS = $(GhcRtsCcOpts)

ifneq "$(way)" "dll"
SRC_HC_OPTS += -static
endif
# SRC_HC_OPTS += -optc-fPIC

ifeq "$(way)" "mp"
SRC_HC_OPTS += -I$$PVM_ROOT/include
endif

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
CC=$(GHC_INPLACE) $(HC_OPTS) $($*_HC_OPTS)
HC=$(GHC_INPLACE)

# prevent this value from leaking into the GMP makefile
unexport CC

# -----------------------------------------------------------------------------
#
#  Building DLLs is only supported on mingw32 at the moment.
# 
DLL_NAME          = HSrts.dll
ifeq "$(way)" "dll"
DLL_IMPLIB_NAME   = libHSrts_imp.a
SRC_BLD_DLL_OPTS += --def HSrts.def -lwinmm -lHS_imp_stub -lgmp -L. -Lgmp

#
# Need an import library containing the symbols the RTS uses from the Prelude.
# So, to avoid bootstrapping trouble, we build one containing just the syms
# we need. Weirdly named to avoid clashing later on when compiling the contents
# of ghc/lib/..
#
# Note: if you do change the name of the Prelude DLL, the "--dllname <nm>.dll"
# below will need to be updated as well.

$(DLL_NAME) :: libHS_imp_stub.a

libHS_imp_stub.a :
	dlltool --output-lib libHS_imp_stub.a --def HSprel.def --dllname HSprel.dll

# It's not included in the DLL, but we need to compile it up separately.
all :: Main.dll_o

endif

# -----------------------------------------------------------------------------
# Compile GMP only if we don't have it already
#
# We use GMP's own configuration stuff, because it's all rather hairy
# and not worth re-implementing in our Makefile framework.

ifneq "$(HaveLibGmp)" "YES"
boot ::
	if [ ! -f gmp/configure ] ; then ( cd gmp && echo ${SED} && autoconf SED= ); fi;
	cd gmp && ./configure --target=$(HOSTPLATFORM)
# Slight cheatage here to past host as target, but x-compilation isn't supported by ghc.

all :: gmp/libgmp.a

install :: gmp/libgmp.a

clean ::
	$(MAKE) -C gmp MAKEFLAGS= clean

ifeq "$(way)" ""
INSTALL_LIBS += gmp/libgmp.a
endif

gmp/libgmp.a ::
	$(MAKE) -C gmp MAKEFLAGS=
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
ifeq "$(EnableWin32DLLs)" "YES"
INSTALL_PROGS += $(DLL_NAME) gmp/gmp.dll
INSTALL_LIBS += $(patsubst %.a, %_imp.a, $(LIBRARY))
INSTALL_LIBS += gmp/libgmp_imp.a Main.dll_o
endif

include $(TOP)/mk/target.mk

