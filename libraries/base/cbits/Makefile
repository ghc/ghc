# $Id: Makefile,v 1.14 2005/02/01 00:52:22 ross Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)

UseGhcForCc = YES

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
EXCLUDED_SRCS += consUtils.c
endif

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB 
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_RTS_DIR) -I../include

LIBRARY = libHSbase_cbits.a
LIBOBJS = $(C_OBJS)

include $(TOP)/mk/target.mk
