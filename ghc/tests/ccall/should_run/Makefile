#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 2000/06/12 17:01:57 panne Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint -fglasgow-exts

callback_HC_OPTS += -fvia-C

callback_LD_OPTS = callback_stub.o
fed001_LD_OPTS = fed001_stub.o

include $(TOP)/mk/target.mk

