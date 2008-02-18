TOP=../..
include $(TOP)/mk/boilerplate.mk

# We have two version: the inplace version compiled by the bootstrap compiler
#   and the install version compiled by the stage 1 compiler
ifeq "$(stage)" "2"
HS_PROG		= hpc$(exeext)
else
HS_PROG		= hpc-inplace$(exeext)
endif
INSTALL_PROGS  += $(HS_PROG)
HPC_LIB         = $(TOP)/libraries/hpc

include $(GHC_COMPAT_DIR)/compat.mk
SRC_HC_OPTS += $(PACKAGE_HPC) -cpp 

ifeq "$(ghc_ge_607)" "YES"
SRC_HC_OPTS += -package containers
endif

binary-dist:
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/hpc
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/hpc/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/hpc/

include $(TOP)/mk/target.mk
