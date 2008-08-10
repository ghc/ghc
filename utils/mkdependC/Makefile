#
# Boilerplate Makefile for building perl script that
# needs some configured constants prepended to it.
#
TOP=../..
include $(TOP)/mk/boilerplate.mk

boot :: all

PROG=mkdependC
CLEAN_FILES += $(PROG)
all::
	$(RM) -f $(PROG)
	echo '#!$(PERL)'                               >> $(PROG)
	echo '$$DEFAULT_TMPDIR = "$(DEFAULT_TMPDIR)";' >> $(PROG)
	echo '$$CPP            = "$(CPP)";'            >> $(PROG)
	echo '$$BUILDPLATFORM  = "$(BUILDPLATFORM)";'  >> $(PROG)
	cat $(PROG).prl                                >> $(PROG)
	$(EXECUTABLE_FILE) $(PROG)

binary-dist:
	@:

include $(TOP)/mk/target.mk
