TOP = ../../../../..
GhcRunTestRules = YES
# These options apply to all tests
RUNSTDTEST_OPTS = -noC -ddump-tc -x1
include $(TOP)/ghc/mk/ghc.mk

runtests :: $(patsubst %.hs, %.runtest, $(wildcard *.hs))

tcfail045_flags = -fglasgow-exts
tcfail059_flags = -hi		     
tcfail060_flags = -hi
tcfail061_flags = -hi
tcfail062_flags = -hi
tcfail063_flags = -hi
tcfail064_flags = -hi
tcfail065_flags = -hi
tcfail066_flags = -hi
tcfail067_flags = -hi
tcfail068_flags = -fglasgow-exts
