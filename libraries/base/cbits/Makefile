# $Id: Makefile,v 1.7 2002/02/14 15:14:02 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)

ifneq "$(HavePosixRegex)" "YES"
ALL_DIRS      += regex
SRC_CC_OPTS   += -Iregex
EXCLUDED_SRCS += regex/engine.c
endif

EXCLUDED_SRCS += ilxstubs.c

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB 
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR) -I../include

LIBRARY = libHSbase_cbits.a
LIBOBJS = $(C_OBJS)

include $(TOP)/mk/target.mk
