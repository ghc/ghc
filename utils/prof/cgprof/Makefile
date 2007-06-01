TOP=../../..
include $(TOP)/mk/boilerplate.mk

C_PROG = cgprof
INSTALL_LIBEXECS=$(C_PROG)

SRC_CC_OPTS += -Wall -I$(GHC_INCLUDE_DIR)

binary-dist:
	$(INSTALL_DIR)            $(BIN_DIST_DIR)/utils/prof/cgprof
	$(INSTALL_DATA) Makefile  $(BIN_DIST_DIR)/utils/prof/cgprof/
	$(INSTALL_DATA) $(C_PROG) $(BIN_DIST_DIR)/utils/prof/cgprof/

include $(TOP)/mk/target.mk
