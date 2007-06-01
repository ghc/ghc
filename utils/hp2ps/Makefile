TOP=../..
include $(TOP)/mk/boilerplate.mk

C_PROG 	= hp2ps

SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -Wall

INSTALL_PROGS += $(C_PROG)

LIBS	     = $(LIBM)

CLEAN_FILES += $(C_OBJS) $(C_PROG)

binary-dist:
	$(INSTALL_DIR)               $(BIN_DIST_DIR)/utils/hp2ps
	$(INSTALL_DATA)    Makefile  $(BIN_DIST_DIR)/utils/hp2ps/
	$(INSTALL_PROGRAM) $(C_PROG) $(BIN_DIST_DIR)/utils/hp2ps/

include $(TOP)/mk/target.mk
