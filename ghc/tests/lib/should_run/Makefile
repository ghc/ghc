#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.13 1999/08/04 10:07:50 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint

packedstring001_HC_OPTS = -syslib misc

exceptions001_HC_OPTS   = -fglasgow-exts -fno-warn-missing-methods
stableptr002_HC_OPTS    = -fglasgow-exts
stableptr003_HC_OPTS    = -fglasgow-exts
list001_HC_OPTS         = -fglasgow-exts

enum01_HC_OPTS		= -cpp -fglasgow-exts -H12m
enum02_HC_OPTS		= -cpp -fglasgow-exts -H12m
enum03_HC_OPTS		= -cpp -fglasgow-exts -H12m

stableptr001_RUNTEST_OPTS = +RTS -K4m
dynamic001_HC_OPTS = -syslib exts

ioexts001_HC_OPTS 	= -fglasgow-exts -O
ioexts001_RUNTEST_OPTS  = +RTS -K8m

memo001_HC_OPTS		= -syslib misc
# stress the garbage collector a bit, to make sure weak pointers are being
# finalized properly, and stable names are GC'd etc.
memo001_RUNTEST_OPTS    = +RTS -A10k -G1

memo002_HC_OPTS		= -syslib misc
memo002_RUNTEST_OPTS	= 20

SRC_MKDEPENDHS_OPTS += -syslib misc -syslib exts 

include $(TOP)/mk/target.mk

