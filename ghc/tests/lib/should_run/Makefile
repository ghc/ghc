#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.12 1999/06/09 09:35:58 simonpj Exp $

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

SRC_MKDEPENDHS_OPTS += -syslib misc -syslib exts 

include $(TOP)/mk/target.mk

