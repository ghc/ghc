#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.7 2000/06/12 16:27:00 panne Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
# conc020 *should* work
OMITTED_RUNTESTS = conc020.run conc021.run
endif

include $(TOP)/mk/should_run.mk

conc009_RUNTEST_OPTS = -x 1
conc021_RUNTEST_OPTS = -x 250

HC_OPTS += -dcore-lint -package concurrent -fglasgow-exts

include $(TOP)/mk/target.mk
