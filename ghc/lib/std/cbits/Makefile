# $Id: Makefile,v 1.30 2001/07/05 13:01:16 rrt Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

PACKAGE = std
IS_CBITS_LIB = YES

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB

ifeq "$(DLLized)" "YES"
SRC_CC_OPTS += -dynamic
endif

# -----------------------------------------------------------------------------
# Installation

INSTALL_DATAS += lockFile.h

include $(TOP)/mk/target.mk
