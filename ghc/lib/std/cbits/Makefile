# $Id: Makefile,v 1.7 1999/10/29 13:52:30 sof Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(filter dll,$(WAYS))" "dll"
override WAYS=dll
else
override WAYS=
endif

LIBRARY=libHS_cbits$(_way).a
INSTALL_LIBS+=$(LIBRARY)

C_SRCS= $(wildcard *.c)

C_OBJS  = $(C_SRCS:.c=.$(way_)o)
LIBOBJS = $(C_OBJS)
SRC_CC_OPTS += -O -I$(GHC_INCLUDE_DIR) $(GhcLibCcOpts)

ifneq "$(way)" "dll"
SRC_CC_OPTS += -static
endif

ifeq "$(way)" "dll"
all :: DllVersionInfo.o

$(DLL_NAME) : DllVersionInfo.o
endif

DLL_NAME = HScbits.dll
DLL_IMPLIB_NAME = libHScbits_imp.a
DLL_DESCRIPTION = "Haskell Prelude helpers"
SRC_BLD_DLL_OPTS += --export-all --output-def=HScbits.def DllVersionInfo.o
SRC_BLD_DLL_OPTS += -lwinmm -lwsock32 -lHSrts_imp -lgmp -L. -L../../../rts/gmp -L../../../rts

#
# Compile the files using the Haskell compiler (ghc really).
# 
CC=$(GHC)

SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

ifeq "$(EnableWin32DLLs)" "YES"
INSTALL_PROGS  += $(DLL_NAME)
INSTALL_LIBS += $(patsubst %.a, %_imp.a, $(LIBRARY))
endif

include $(TOP)/mk/target.mk
