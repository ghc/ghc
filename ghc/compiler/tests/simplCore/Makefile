TOP = ../../../..
GhcRunTestRules = YES
# These options apply to all tests
RUNSTDTEST_OPTS = -noC -O -ddump-simpl -dcore-lint
include $(TOP)/ghc/mk/ghc.mk

runtests :: $(patsubst %.hs, %.runtest, $(wildcard *.hs))
