TOP=..
include $(TOP)/mk/boilerplate.mk

SCRIPT_PROG=fastmake
SCRIPT_OBJS=fastmake.prl

#
# Prepend (perl) bindings for these Makefile variables
# when creating the final script
#
SCRIPT_SUBST_VARS=TMPDIR

INTERP=$(PERL)
# ToDo: install
DESTDIR=$(INSTSCRIPTDIR)
CLEAN_FILES += $(SCRIPT_PROG)

include $(TOP)/mk/target.mk

