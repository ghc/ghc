#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.8 2000/12/12 17:21:41 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint
arith006_HC_OPTS +=  -package lang
arith010_HC_OPTS +=  -package lang
arith011_HC_OPTS +=  -package lang
arith012_HC_OPTS +=  -package lang
num006_HC_OPTS +=  -package lang

.PRECIOUS: %.bin %.o

SRC_MKDEPENDHS_OPTS += -fglasgow-exts

include $(TOP)/mk/target.mk
