#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 2000/05/10 14:30:59 rrt Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
# conc020 *should* work
OMITTED_RUNTESTS = conc020.run conc021.run
endif

include $(TOP)/mk/should_run.mk

conc009_RUNTEST_OPTS = -x 1
conc021_RUNTEST_OPTS = -x 250

SRC_HC_OPTS += -dcore-lint -syslib concurrent -fglasgow-exts

include $(TOP)/mk/target.mk
