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

binary-dist:
	$(INSTALL_DIR)              $(BIN_DIST_DIR)/utils/mkdirhier
	$(INSTALL_DATA)    Makefile $(BIN_DIST_DIR)/utils/mkdirhier/
	$(INSTALL_PROGRAM) $(PROG)  $(BIN_DIST_DIR)/utils/mkdirhier/

include $(TOP)/mk/target.mk
