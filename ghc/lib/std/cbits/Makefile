# $Id: Makefile,v 1.3 1999/03/02 20:14:00 sof Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk
override WAYS=

LIBRARY=libHS_cbits.a
INSTALL_LIBS+=$(LIBRARY)

C_SRCS= $(wildcard *.c)

C_OBJS  = $(C_SRCS:.c=.o)
LIBOBJS = $(C_OBJS)
SRC_CC_OPTS += -O -I$(GHC_INCLUDE_DIR) $(GhcLibCcOpts)


#
# Compile the files using the Haskell compiler (ghc really).
# 
CC=$(HC)

SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

include $(TOP)/mk/target.mk
