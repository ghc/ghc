# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 2001/04/24 23:51:11 andy Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint

arr014_HC_OPTS = -package lang
arr016_HC_OPTS = -fglasgow-exts

arr003_RUNTEST_OPTS = -x 1
arr004_RUNTEST_OPTS = -x 1
arr007_RUNTEST_OPTS = -x 1
arr008_RUNTEST_OPTS = -x 1

# mkdependHS doesn't understand OPTIONS pragmas...
SRC_MKDEPENDHS_OPTS += -package lang

include $(TOP)/mk/target.mk
