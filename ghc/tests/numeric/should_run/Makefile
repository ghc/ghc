#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 1999/11/01 16:07:50 simonpj Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

HC_OPTS += -dcore-lint
arith006_HC_OPTS += -fglasgow-exts
arith010_HC_OPTS += -fglasgow-exts
arith011_HC_OPTS += -fglasgow-exts
arith012_HC_OPTS += -fglasgow-exts
num006_HC_OPTS += -fglasgow-exts

.PRECIOUS: %.bin %.o

SRC_MKDEPENDHS_OPTS += -fglasgow-exts

include $(TOP)/mk/target.mk
