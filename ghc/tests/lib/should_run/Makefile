#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 1998/08/14 13:02:00 simonm Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint

packedstring001_HC_OPTS = -syslib misc
exceptions001_HC_OPTS   = -fglasgow-exts
stableptr002_HC_OPTS   = -fglasgow-exts

stableptr001_RUNTEST_OPTS = +RTS -K4m
dynamic001_HC_OPTS = -syslib exts

SRC_MKDEPENDHS_OPTS += -syslib misc -syslib exts 

include $(TOP)/mk/target.mk

