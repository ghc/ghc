# $Id: Makefile,v 1.10 2004/08/16 11:07:31 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)

UseGhcForCc = YES

# If there is no system-supplied POSIX regex library, use our own
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
SRC_CC_OPTS += -I$(GHC_RUNTIME_DIR) -I../include

LIBRARY = libHSbase_cbits.a
LIBOBJS = $(C_OBJS)

include $(TOP)/mk/target.mk
