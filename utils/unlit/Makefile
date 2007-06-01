TOP=../..
include $(TOP)/mk/boilerplate.mk

C_SRCS=unlit.c
C_PROG=unlit
SRC_CC_OPTS += -O

# Get it over with!
boot :: all

#
# Install unlit in lib/
#
INSTALL_LIBEXECS += $(C_PROG)

binary-dist:
	$(INSTALL_DIR)               $(BIN_DIST_DIR)/utils/unlit
	$(INSTALL_DATA)    Makefile  $(BIN_DIST_DIR)/utils/unlit/
	$(INSTALL_PROGRAM) $(C_PROG) $(BIN_DIST_DIR)/utils/unlit/

include $(TOP)/mk/target.mk
