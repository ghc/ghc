#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.8 1998/11/25 16:44:05 simonm Exp $

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

ioexts001_HC_OPTS 	= -fglasgow-exts -O
ioexts001_RUNTEST_OPTS  = +RTS -K8m

SRC_MKDEPENDHS_OPTS += -syslib misc -syslib exts 

include $(TOP)/mk/target.mk

