# $Id: Makefile,v 1.1 2001/06/28 14:15:04 simonmar Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

HSLIB = core
IS_CBITS_LIB = YES

SRC_CC_OPTS += -Wall -DCOMPILING_STDLIB -I../include -I../../../ghc/includes -I../../../ghc/rts

ifeq "$(DLLized)" "YES"
SRC_CC_OPTS += -dynamic
endif

# -----------------------------------------------------------------------------
# Installation

INSTALL_DATAS += lockFile.h

include $(TOP)/mk/target.mk
