# $Id: Makefile,v 1.2 2001/07/31 11:51:09 simonmar Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

PACKAGE = core
IS_CBITS_LIB = YES

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB -I../include -I../../../ghc/includes -I../../../ghc/rts

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
