#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.22 2000/06/12 17:01:58 panne Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint

packedstring001_HC_OPTS = -package lang
exceptions001_HC_OPTS   = -fglasgow-exts -fno-warn-missing-methods
stableptr001_HC_OPTS    = -fglasgow-exts
stableptr002_HC_OPTS    = -fglasgow-exts
stableptr003_HC_OPTS    = -fglasgow-exts
list001_HC_OPTS         = -fglasgow-exts
uri001_HC_OPTS		= -package lang -package net
time001_HC_OPTS	        = -package lang

enum01_HC_OPTS		= -cpp -fglasgow-exts -H12m
enum02_HC_OPTS		= -cpp -fglasgow-exts -H12m
enum03_HC_OPTS		= -cpp -fglasgow-exts -H12m

stableptr001_RUNTEST_OPTS = +RTS -K4m
dynamic001_HC_OPTS = -package lang

io001_HC_OPTS = -fglasgow-exts
io002_HC_OPTS = -fglasgow-exts
addr001_HC_OPTS = -fglasgow-exts

ioexts001_HC_OPTS 	= -fglasgow-exts -O
ioexts001_RUNTEST_OPTS  = +RTS -K8m

memo001_HC_OPTS		= -package lang -package util
# stress the garbage collector a bit, to make sure weak pointers are being
# finalized properly, and stable names are GC'd etc.
memo001_RUNTEST_OPTS    = +RTS -A10k -G1

memo002_HC_OPTS		= -package lang -package util
memo002_RUNTEST_OPTS	= 20

weak001_HC_OPTS		= -fglasgow-exts

SRC_MKDEPENDHS_OPTS += -package lang

include $(TOP)/mk/target.mk

