# $Id: Makefile,v 1.25 2001/03/01 12:25:33 rrt Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

override WAYS=

HSLIB = std
IS_CBITS_LIB = YES

C_SRCS= $(wildcard *.c)

C_OBJS  = $(C_SRCS:.c=.$(way_)o)
LIBOBJS = $(C_OBJS)
SRC_CC_OPTS += -O $(GhcLibCcOpts) -Wall -optc-DCOMPILING_STDLIB
ifeq "$(DLLized)" "YES"
SRC_CC_OPTS += -dynamic
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
