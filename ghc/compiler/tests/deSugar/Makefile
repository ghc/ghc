TOP = ../../../..
GhcRunTestRules = YES
# These options apply to all tests
RUNSTDTEST_OPTS = -noC -ddump-ds -dcore-lint
include $(TOP)/ghc/mk/ghc.mk

runtests :: $(patsubst %.hs, %.runtest, $(wildcard *.hs))
	@echo 'TODO: ds014a -- some things that should NOT go through'

#SUBDIRS = cvh-ds-unboxed

ds030_flags = -dppr-all
ds035_flags = -fglasgow-exts
ds039_flags = -dppr-all
