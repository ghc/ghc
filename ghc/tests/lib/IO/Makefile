# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.8 2001/06/07 10:45:30 sewardj Exp $

TOP = ../..

include $(TOP)/mk/boilerplate.mk

include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint -cpp

hGetBuffering001_RUNTEST_OPTS += -i hGetBuffering001.hs
hSetBuffering002_RUNTEST_OPTS += -i hSetBuffering002.hs
hSetBuffering003_RUNTEST_OPTS += -i hSetBuffering003.hs
misc001_RUNTEST_OPTS          += misc001.hs misc001.out
hGetChar001_RUNTEST_OPTS      += -i hGetChar001.stdin
openFile002_RUNTEST_OPTS      += -x 1
IOError001_RUNTEST_OPTS       += -i IOError001.hs -o1 IOError001.stdout-mingw
readwrite002_RUNTEST_OPTS     += -i readwrite002.hs
hGetLine001_RUNTEST_OPTS      += -i hGetLine001.hs -o1 hGetLine001.stdout-mingw
hFileSize001_RUNTEST_OPTS     += -o1 hFileSize001.stdout-mingw
hSeek004_RUNTEST_OPTS         += -o1 hSeek004.stdout-mingw
hGetPosn001_RUNTEST_OPTS      += -o1 hGetPosn001.stdout-mingw
openFile003_RUNTEST_OPTS      += -o1 openFile003.stdout-mingw
openFile005_RUNTEST_OPTS      += -o1 openFile005.stdout-mingw

.PRECIOUS: %.o %.bin

CLEAN_FILES += *.out* *.inout

include $(TOP)/mk/target.mk
