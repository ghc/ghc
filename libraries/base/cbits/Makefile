# $Id: Makefile,v 1.4 2002/02/06 10:40:26 simonmar Exp $

UseGhcForCc = YES

TOP = ../..
include $(TOP)/mk/boilerplate.mk

HC = $(GHC_INPLACE)

EXCLUDED_SRCS += ilxstubs.c

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB -I$(GHC_RUNTIME_DIR)

LIBRARY = libHSbase_cbits.a
LIBOBJS = $(C_OBJS)

include $(TOP)/mk/target.mk
