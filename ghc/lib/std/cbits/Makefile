# $Id: Makefile,v 1.20 2000/05/31 12:04:49 panne Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

WAYS=$(GhcLibWays)

ifeq "$(filter dll,$(WAYS))" "dll"
override WAYS=dll
else
override WAYS=
endif

LIBRARY=libHSstd_cbits$(_way).a

C_SRCS= $(wildcard *.c)

C_OBJS  = $(C_SRCS:.c=.$(way_)o)
LIBOBJS = $(C_OBJS)
SRC_CC_OPTS += -O -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR) $(GhcLibCcOpts) -Wall

ifneq "$(way)" "dll"
SRC_CC_OPTS += -static
endif

ifeq "$(way)" "dll"
all :: DllVersionInfo.o

$(DLL_NAME) : DllVersionInfo.o
endif

DLL_NAME = HSstd_cbits.dll
DLL_IMPLIB_NAME = libHSstd_cbits_imp.a
DLL_DESCRIPTION = "Haskell Prelude helpers"
SRC_BLD_DLL_OPTS += --export-all --output-def=HSstdcbits.def DllVersionInfo.o
SRC_BLD_DLL_OPTS += -lwinmm -lwsock32 -lHSrts_imp -lgmp -L. -L../../../rts/gmp -L../../../rts

#
# Compile the files using the Haskell compiler (ghc really).
# 
CC=$(GHC_INPLACE)

# ghc-inplace needs access to HsStd.h and its includes, so copy them into the
# standard place.
# NOTE 1: Installation of the header files into their final place is done via
# GHC_INCLUDE_DIR. This is not nice, but there is no easy way out.
# NOTE 2: Filtering out timezone.h is a little bit hacky, but we don't need it
# after compilation.
boot ::
	cp $(filter-out timezone.h,$(wildcard *.h)) $(GHC_INCLUDE_DIR)

CLEAN_FILES += $(foreach header_file, $(filter-out timezone.h,$(wildcard *.h)), $(GHC_INCLUDE_DIR)/$(header_file))

SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

# -----------------------------------------------------------------------------
# Installation

INSTALL_LIBS+=$(LIBRARY)

ifeq "$(EnableWin32DLLs)" "YES"
INSTALL_PROGS  += $(DLL_NAME)
ifneq "$(way)" "dll"
INSTALL_LIBS += $(patsubst %.a, %_imp.a, $(LIBRARY))
endif
endif

include $(TOP)/mk/target.mk
