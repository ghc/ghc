TOP = ../../../..
GhcRunTestRules = YES
# These options apply to all tests
RUNSTDTEST_OPTS = -ddump-rn -noC
include $(TOP)/ghc/mk/ghc.mk

runtests :: $(patsubst %.hs, %.runtest, $(wildcard *.hs))

#SUBDIRS = bevan-bug-1

rn001_flags = -x1
rn002_flags = -x1
rn004_flags = -x1
rn007_flags = -x1
rn008_flags = -x1
rn015_flags = -x1
rn017_flags = -hi
