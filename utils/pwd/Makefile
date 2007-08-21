TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG=pwd

binary-dist:
	$(INSTALL_DIR)                      $(BIN_DIST_DIR)/utils/pwd
	$(INSTALL_DATA)    Makefile         $(BIN_DIST_DIR)/utils/pwd/
	$(INSTALL_PROGRAM) pwd$(exeext)     $(BIN_DIST_DIR)/utils/pwd/

include $(TOP)/mk/target.mk
