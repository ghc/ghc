# $Id: Makefile,v 1.32 2001/08/17 00:17:55 sof Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

PACKAGE = std
IS_CBITS_LIB = YES

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR)

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
