#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 2000/04/26 10:03:28 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

conc009_RUNTEST_OPTS = -x 1
conc021_RUNTEST_OPTS = -x 250

SRC_HC_OPTS += -dcore-lint -syslib concurrent -fglasgow-exts

include $(TOP)/mk/target.mk
