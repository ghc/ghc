# $Id: Makefile,v 1.3 2001/08/17 12:50:34 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

PACKAGE = core
IS_CBITS_LIB = YES

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB -I../include -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR)

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
