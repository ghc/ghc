# $Id: Makefile,v 1.1 1998/02/02 17:34:22 simonm Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk
override WAYS=

LIBRARY=libHS_cbits.a
INSTALL_LIBS+=$(LIBRARY)

SRCS= $(wildcard *.lc)

C_SRCS  = $(SRCS:.lc=.c)
C_OBJS  = $(C_SRCS:.c=.o)
LIBOBJS = $(C_OBJS)
SRC_CC_OPTS = -O -I$(GHC_INCLUDE_DIR)

#
# Compile the files using the Haskell compiler (ghc really).
# 
CC=$(HC)

#
# Remove the intermediate .c files
# (the .o's will be removed automatically by default mk setup)
#
CLEAN_FILES += $(C_SRCS)

SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

include $(TOP)/mk/target.mk
