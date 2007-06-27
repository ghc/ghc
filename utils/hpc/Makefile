TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG		= hpc$(exeext)
INSTALL_PROGS  += $(HS_PROG)
HPC_LIB         = $(TOP)/libraries/hpc

# This causes libghccompat.a to be used:
include $(GHC_COMPAT_DIR)/compat.mk

# This is required because libghccompat.a must be built with
# $(GhcHcOpts) because it is linked to the compiler, and hence
# we must also build with $(GhcHcOpts) here:
SRC_HC_OPTS += $(GhcHcOpts) $(GhcStage1HcOpts)

binary-dist:
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/hpc
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/hpc/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/hpc/

include $(TOP)/mk/target.mk
