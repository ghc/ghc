TOP = ../../../..
GhcRunTestRules = YES
# These options apply to all tests
RUNSTDTEST_OPTS = -fglasgow-exts -dcore-lint
include $(TOP)/ghc/mk/ghc.mk

runtests :: $(patsubst %.hs, %.runtest, $(wildcard *.hs))

# Note that these tests are still in a state of flux... don't believe errors
# they report. In fact, these aren't really very good tests at all...

cc001_flags = -noC -ddump-tc -ddump-ds 
cc002_flags = -noC -ddump-tc -ddump-ds 
cc003_flags = -noC -ddump-tc -ddump-ds -x1
cc004_flags = -noC -ddump-tc -ddump-ds 
cc005_flags = -via-C -ddump-stg -ddump-flatC
cc006_flags = -via-C -ddump-stg -ddump-flatC
cc007_flags = -via-C -ddump-stg -ddump-flatC
