# $Id: Makefile,v 1.34 2001/12/02 15:47:08 panne Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

PACKAGE = std
IS_CBITS_LIB = YES

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR)

ifeq "$(ILXized)" "YES"
DLLized = YES
C_SRCS += $(FPTOOLS_TOP)/ghc/rts/StgPrimFloat.c
else
EXCLUDED_SRCS += ilxstubs.c
endif

# -----------------------------------------------------------------------------
# Installation

INSTALL_DATAS += HsStd.h lockFile.h

include $(TOP)/mk/target.mk
