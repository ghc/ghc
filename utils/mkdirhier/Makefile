TOP=../..
include $(TOP)/mk/boilerplate.mk

boot :: all

PROG=mkdirhier
CLEAN_FILES += $(PROG)
all::
	$(RM) -f $(PROG)
	echo '#!$(SHELL)'                              >> $(PROG)
	cat $(PROG).sh                                 >> $(PROG)
	$(EXECUTABLE_FILE) $(PROG)

include $(TOP)/mk/target.mk
