TOP=../../..
include $(TOP)/mk/boilerplate.mk

# Exclude for booting
ifeq "$(stage)" "2"
SRC_CC_OPTS += $(MACOSX_DEPLOYMENT_CC_OPTS)
SRC_LD_OPTS += $(MACOSX_DEPLOYMENT_LD_OPTS)
endif

C_PROG = cgprof
INSTALL_LIBEXECS=$(C_PROG)

SRC_CC_OPTS += -Wall -I$(GHC_INCLUDE_DIR)

binary-dist:
	$(INSTALL_DIR)            $(BIN_DIST_DIR)/utils/prof/cgprof
	$(INSTALL_DATA) Makefile  $(BIN_DIST_DIR)/utils/prof/cgprof/
	$(INSTALL_DATA) $(C_PROG) $(BIN_DIST_DIR)/utils/prof/cgprof/

include $(TOP)/mk/target.mk
