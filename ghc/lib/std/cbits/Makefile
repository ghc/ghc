# $Id: Makefile,v 1.24 2000/11/07 10:42:57 simonmar Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

override WAYS=

HSLIB = std
IS_CBITS_LIB = YES

C_SRCS= $(wildcard *.c)

C_OBJS  = $(C_SRCS:.c=.$(way_)o)
LIBOBJS = $(C_OBJS)
SRC_CC_OPTS += -O $(GhcLibCcOpts) -Wall -optc-DCOMPILING_STDLIB
ifneq "$(DLLized)" "YES"
SRC_CC_OPTS += -static
endif

#
# Compile the files using the Haskell compiler (ghc really).
# 
CC=$(GHC_INPLACE)

# -----------------------------------------------------------------------------
# Installation

override datadir:=$(libdir)/includes
INSTALL_DATAS += HsStd.h stgio.h stgerror.h fileObject.h

include $(TOP)/mk/target.mk
