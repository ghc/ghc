TOP = ../../../../..
GhcRunTestRules = YES
# These options apply to all tests
RUNSTDTEST_OPTS = -noC -ddump-tc -dcore-lint -hi
include $(TOP)/ghc/mk/ghc.mk

runtests :: $(patsubst %.hs, %.runtest, $(wildcard *.hs))

tc019_flags = -fglasgow-exts
