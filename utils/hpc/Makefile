TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG		= hpc$(exeext)
INSTALL_PROGS  += $(HS_PROG)
HPC_LIB         = $(TOP)/libraries/hpc

include $(GHC_COMPAT_DIR)/compat.mk
SRC_HC_OPTS += $(PACKAGE_HPC) -cpp 

binary-dist:
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/hpc
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/hpc/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/hpc/

include $(TOP)/mk/target.mk
