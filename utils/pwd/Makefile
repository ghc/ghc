TOP=../..
include $(TOP)/mk/boilerplate.mk

# We have two version: the inplace version compiled by the bootstrap compiler
#   and the install version compiled by the stage 1 compiler
ifeq "$(stage)" "2"
HS_PROG           = pwd
else
HS_PROG           = pwd-inplace
endif

binary-dist:
	$(INSTALL_DIR)                      $(BIN_DIST_DIR)/utils/pwd
	$(INSTALL_DATA)    Makefile         $(BIN_DIST_DIR)/utils/pwd/
	$(INSTALL_PROGRAM) pwd$(exeext)     $(BIN_DIST_DIR)/utils/pwd/

include $(TOP)/mk/target.mk
