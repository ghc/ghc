#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.18 2001/03/28 14:26:34 simonmar Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
OMITTED_RUNTESTS = cg030.run cg035.run
endif
ifeq "$(TARGETPLATFORM)" "sparc-sun-solaris2"
OMITTED_RUNTESTS = cg026.run
endif

include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint

cg012_HC_OPTS = -fglasgow-exts
cg015_HC_OPTS = -fglasgow-exts
cg018_HC_OPTS = -fglasgow-exts
cg025_HC_OPTS = -package lang -package text
cg026_HC_OPTS = -package lang -fglasgow-exts -fvia-C
cg029_HC_OPTS = -package lang -fglasgow-exts
cg030_HC_OPTS = -package lang -fglasgow-exts
cg031_HC_OPTS = -fglasgow-exts
cg032_HC_OPTS = -fglasgow-exts
cg033_HC_OPTS = -fglasgow-exts
cg035_HC_OPTS = -package lang -fglasgow-exts
cg042_HC_OPTS = -package lang -fglasgow-exts
cg044_HC_OPTS = -package lang

cg016_RUNTEST_OPTS = -x 1	# should override the -x 0 above.
cg021_RUNTEST_OPTS = +RTS -K2m
cg025_RUNTEST_OPTS = -x 1 cg025.hs
cg045_RUNTEST_OPTS = -x 1

# tmp, until we fix the problems with seq#...
cg045_HC_OPTS = -O

# mkdependHS doesn't understand OPTIONS pragmas...
SRC_MKDEPENDHS_OPTS += -fglasgow-exts

.PRECIOUS: %.bin %.o

CLEAN_FILES += PrelMain.hi

include $(TOP)/mk/target.mk
