#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2000/04/26 10:43:23 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint -fglasgow-exts

callback_LD_OPTS = callback_stub.o
fed001_LD_OPTS = fed001_stub.o

include $(TOP)/mk/target.mk

