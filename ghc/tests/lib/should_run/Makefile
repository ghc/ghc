#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 1998/08/04 15:21:15 simonm Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint

packedstring001_HC_OPTS = -syslib misc
stableptr001_RUNTEST_OPTS = +RTS -K4m

SRC_MKDEPENDHS_OPTS += -syslib misc

include $(TOP)/mk/target.mk

