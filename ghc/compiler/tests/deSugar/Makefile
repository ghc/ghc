TOP = ../../..
include $(TOP)/mk/boilerplate.mk

SRCS = $(wildcard *.hs)
HS_PROG = ds040

PROG : $(SRCS)

runtests :: $(wildcard *.hs)
	@echo 'TODO: ds014a -- some things that should NOT go through'

#SUBDIRS = cvh-ds-unboxed

# These options apply to all tests
HC_OPTS += -noC -ddump-ds -dcore-lint

ds030_HC_OPTS = -dppr-all
ds035_HC_OPTS = -fglasgow-exts
ds039_HC_OPTS = -dppr-all


include $(TOP)/mk/target.mk
