#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.12 2000/06/12 17:01:57 panne Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
OMITTED_RUNTESTS = cg030.run cg035.run
endif

include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint

cg012_HC_OPTS = -fglasgow-exts
cg015_HC_OPTS = -fglasgow-exts
cg018_HC_OPTS = -fglasgow-exts
cg025_HC_OPTS = -fglasgow-exts
cg026_HC_OPTS = -fglasgow-exts
cg029_HC_OPTS = -fglasgow-exts
cg030_HC_OPTS = -fglasgow-exts
cg031_HC_OPTS = -fglasgow-exts
cg032_HC_OPTS = -fglasgow-exts
cg033_HC_OPTS = -fglasgow-exts
cg035_HC_OPTS = -fglasgow-exts
cg042_HC_OPTS = -fglasgow-exts
cg044_HC_OPTS = -fglasgow-exts

cg016_RUNTEST_OPTS = -x 1	# should override the -x 0 above.
cg021_RUNTEST_OPTS = +RTS -K2m
cg025_RUNTEST_OPTS = -x 1 cg025.hs
cg045_RUNTEST_OPTS = -x 1

# mkdependHS doesn't understand OPTIONS pragmas...
SRC_MKDEPENDHS_OPTS += -fglasgow-exts

.PRECIOUS: %.bin %.o

clean ::
	rm -f *.bin *.o

include $(TOP)/mk/target.mk
