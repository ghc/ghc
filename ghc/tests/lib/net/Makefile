# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.1 2001/05/01 11:46:56 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint -package net -fglasgow-exts

.PRECIOUS: %.o %.bin

include $(TOP)/mk/target.mk
