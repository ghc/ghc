# $Id: Makefile,v 1.29 2001/05/18 22:34:42 qrczak Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

HSLIB = std
IS_CBITS_LIB = YES

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB

ifeq "$(DLLized)" "YES"
SRC_CC_OPTS += -dynamic
endif

# -----------------------------------------------------------------------------
# Installation

INSTALL_DATAS += lockFile.h

include $(TOP)/mk/target.mk
