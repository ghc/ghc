#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.7 1998/10/08 11:52:34 simonm Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint

packedstring001_HC_OPTS = -syslib misc
exceptions001_HC_OPTS   = -fglasgow-exts
stableptr002_HC_OPTS    = -fglasgow-exts
list001_HC_OPTS         = -fglasgow-exts

stableptr001_RUNTEST_OPTS = +RTS -K4m
dynamic001_HC_OPTS = -syslib exts

SRC_MKDEPENDHS_OPTS += -syslib misc -syslib exts 

include $(TOP)/mk/target.mk

