#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1999/03/16 13:20:21 simonm Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

conc009_RUNTEST_OPTS = -x 1

SRC_HC_OPTS += -dcore-lint -syslib concurrent -fglasgow-exts

include $(TOP)/mk/target.mk
