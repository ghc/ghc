# $Id: Makefile,v 1.26 2001/03/26 16:56:27 simonmar Exp $

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

INSTALL_DATAS += stgio.h stgerror.h fileObject.h

include $(TOP)/mk/target.mk
