# $Id: Makefile,v 1.6 1999/10/11 08:38:25 simonmar Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk
override WAYS=

LIBRARY=libHS_cbits.a
INSTALL_LIBS+=$(LIBRARY)

C_SRCS= $(wildcard *.c)

C_OBJS  = $(C_SRCS:.c=.o)
LIBOBJS = $(C_OBJS)
SRC_CC_OPTS += -O -I$(GHC_INCLUDE_DIR) $(GhcLibCcOpts)


DLL_NAME = HScbits.dll
DLL_IMPLIB_NAME = libHScbits_imp.a
SRC_BLD_DLL_OPTS += --export-all --output-def=HScbits.def
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
