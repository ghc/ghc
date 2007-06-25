TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG		= hpc$(exeext)
INSTALL_PROGS  += $(HS_PROG)
HPC_LIB         = $(TOP)/libraries/hpc

SRCS += Trace/Hpc/Mix.hs Trace/Hpc/Tix.hs Trace/Hpc/Util.hs

# workaround till we can force hpc to be built with stage-1.
Trace/Hpc/%.hs: $(HPC_LIB)/Trace/Hpc/%.hs
	mkdir -p Trace/Hpc
	cp $(HPC_LIB)/$@ $@

binary-dist:
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/hpc
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/hpc/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/hpc/

include $(TOP)/mk/target.mk
