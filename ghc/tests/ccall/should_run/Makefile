#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 2000/06/12 16:26:59 panne Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

HC_OPTS += -dcore-lint -fglasgow-exts

callback_HC_OPTS += -fvia-C

callback_LD_OPTS = callback_stub.o
fed001_LD_OPTS = fed001_stub.o

include $(TOP)/mk/target.mk

