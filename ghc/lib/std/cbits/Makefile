# $Id: Makefile,v 1.33 2001/08/17 10:10:15 rrt Exp $

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

INSTALL_DATAS += lockFile.h

include $(TOP)/mk/target.mk
