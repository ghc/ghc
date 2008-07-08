TOP=../..
include $(TOP)/mk/boilerplate.mk

# Beyond stage 1, honour any Mac OS X depolyment target options.  If we use 
# these options in stage 1 we get a linker error if the bootstrap compiler is
#  for a more recent OS version
ifeq "$(stage)" "2"
SRC_CC_OPTS += $(MACOSX_DEPLOYMENT_CC_OPTS)
SRC_HC_OPTS += $(addprefix -optc, $(MACOSX_DEPLOYMENT_CC_OPTS))
SRC_LD_OPTS += $(addprefix -optl, $(MACOSX_DEPLOYMENT_LD_OPTS))
endif

# We have two version: the inplace version compiled by the bootstrap compiler
#   and the install version compiled by the stage 1 compiler
ifeq "$(stage)" "2"
HS_PROG		= hpc$(exeext)
else
HS_PROG		= hpc-inplace$(exeext)
endif
INSTALL_PROGS  += $(HS_PROG)
HPC_LIB         = $(TOP)/libraries/hpc

SRC_HC_OPTS += -package hpc -cpp 

ifeq "$(ghc_ge_607)" "YES"
SRC_HC_OPTS += -package containers
endif

binary-dist:
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/hpc
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/hpc/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/hpc/

include $(TOP)/mk/target.mk
