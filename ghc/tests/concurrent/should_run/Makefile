#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.10 2001/02/13 15:12:42 rrt Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
# conc020 *should* work
OMITTED_RUNTESTS = conc020.run conc021.run
endif

include $(TOP)/mk/should_run.mk

conc007_RUNTEST_OPTS = +RTS -H128M -RTS
conc009_RUNTEST_OPTS = -x 1
conc021_RUNTEST_OPTS = -x 250 -o2 conc021.stderr-mingw

SRC_HC_OPTS += -dcore-lint -package concurrent -fglasgow-exts

CLEAN_FILES += PrelMain.hi

include $(TOP)/mk/target.mk
