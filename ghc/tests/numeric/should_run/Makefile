#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 1999/01/24 14:19:43 sof Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

HC_OPTS += -dcore-lint
arith010_HC_OPTS += -fglasgow-exts
arith011_HC_OPTS += -fglasgow-exts
arith012_HC_OPTS += -fglasgow-exts

.PRECIOUS: %.bin %.o

SRC_MKDEPENDHS_OPTS += -fglasgow-exts

include $(TOP)/mk/target.mk
