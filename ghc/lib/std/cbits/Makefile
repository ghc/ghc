# $Id: Makefile,v 1.31 2001/07/13 13:40:26 rrt Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

PACKAGE = std
IS_CBITS_LIB = YES

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB

ifeq "$(ILXized)" "YES"
DLLized = YES
C_SRCS += $(FPTOOLS_TOP)/ghc/rts/StgPrimFloat.c
else
C_SRCS := $(filter-out ilxstubs.c,$(C_SRCS))
endif

# -----------------------------------------------------------------------------
# Installation

INSTALL_DATAS += lockFile.h

include $(TOP)/mk/target.mk
