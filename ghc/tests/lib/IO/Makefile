# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2001/05/21 15:25:24 simonmar Exp $

TOP = ../..

include $(TOP)/mk/boilerplate.mk

include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint

hSetBuffering002_RUNTEST_OPTS += -i hSetBuffering002.hs
hSetBuffering003_RUNTEST_OPTS += -i hSetBuffering003.hs
misc001_RUNTEST_OPTS          += misc001.hs misc001.out
hGetChar001_RUNTEST_OPTS      += -i hGetChar001.stdin
openFile002_RUNTEST_OPTS      += -x 1
IOError001_RUNTEST_OPTS       += -o1 IOError001.stdout-mingw
readwrite002_RUNTEST_OPTS     += -i readwrite002.hs
hGetLine001_RUNTEST_OPTS      += -i hGetLine001.hs

.PRECIOUS: %.o %.bin

CLEAN_FILES += *.out* *.inout

include $(TOP)/mk/target.mk
