#
#

TOP=..
include $(TOP)/mk/boilerplate.mk

SCRIPT_PROG=runstdtest
SCRIPT_OBJS=runstdtest.prl
SCRIPT_SUBST_VARS=RM TMPDIR CONTEXT_DIFF
INTERP=$(PERL)

CLEAN_FILES += $(SCRIPT_PROG)
DESTDIR=$(INSTSCRIPTDIR)

include $(TOP)/mk/target.mk
