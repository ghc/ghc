TOP = ../../..
include $(TOP)/mk/boilerplate.mk

runtests :: $(patsubst %.hs, %.runtest, $(wildcard *.hs))
	@echo 'TODO: ds014a -- some things that should NOT go through'

#SUBDIRS = cvh-ds-unboxed

# These options apply to all tests
RUN_TEST_OPTS = -noC -ddump-ds -dcore-lint

ds030_flags = -dppr-all
ds035_flags = -fglasgow-exts
ds039_flags = -dppr-all


include $(TOP)/mk/target.mk
