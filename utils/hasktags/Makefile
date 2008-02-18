TOP=../..
include $(TOP)/mk/boilerplate.mk

# We have two version: the inplace version compiled by the bootstrap compiler
#   and the install version compiled by the stage 1 compiler
ifeq "$(stage)" "2"
HS_PROG           = hasktags
else
HS_PROG           = hasktags-inplace
endif

CLEAN_FILES += Main.hi

SRC_HC_OPTS += -Wall

INSTALL_PROGS += $(HS_PROG)

binary-dist:
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/hasktags
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/hasktags/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/hasktags/

include $(TOP)/mk/target.mk
