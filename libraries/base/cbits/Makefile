# $Id: Makefile,v 1.8 2002/07/18 22:01:07 sof Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)

ifneq "$(HavePosixRegex)" "YES"
ALL_DIRS      += regex
SRC_CC_OPTS   += -Iregex
EXCLUDED_SRCS += regex/engine.c
endif

EXCLUDED_SRCS += ilxstubs.c

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
EXCLUDED_SRCS += consUtils.c
endif

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB 
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR) -I../include

LIBRARY = libHSbase_cbits.a
LIBOBJS = $(C_OBJS)

include $(TOP)/mk/target.mk
