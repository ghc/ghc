TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG = hasktags

CLEAN_FILES += Main.hi

INSTALL_PROGS += $(HS_PROG)

binary-dist:
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/hasktags
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/hasktags/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/hasktags/

include $(TOP)/mk/target.mk
