# $Id: Makefile,v 1.27 2001/05/18 21:19:11 qrczak Exp $

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

include $(TOP)/mk/target.mk
