#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2000/03/13 11:39:22 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

conc009_RUNTEST_OPTS = -x 1
conc018_RUNTEST_OPTS = -x 1

SRC_HC_OPTS += -dcore-lint -syslib concurrent -fglasgow-exts

include $(TOP)/mk/target.mk
