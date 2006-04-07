#
# Boilerplate Makefile for building perl script that
# needs some configured constants prepended to it.
#
TOP=../..
include $(TOP)/mk/boilerplate.mk

boot :: all

SCRIPT_PROG=mkdependC
SCRIPT_OBJS=mkdependC.prl
#
# Prepend (perl) bindings for these Makefile variables
# when creating `mkdependC' (a more flexible way of doing msub).
#
SCRIPT_SUBST_VARS=DEFAULT_TMPDIR CPP BUILDPLATFORM

CLEAN_FILES += $(SCRIPT_PROG)
INTERP=perl

include $(TOP)/mk/target.mk
