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
HS_PROG           = runghc$(exeext)
else
HS_PROG           = runghc-inplace$(exeext)
endif
INSTALL_PROGS   += $(HS_PROG)

UseGhcForCc = YES
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

SRC_HC_OPTS += -Wall

RUNHASKELL_PROG = runhaskell$(exeext)
all :: $(RUNHASKELL_PROG)

$(RUNHASKELL_PROG) : $(HS_PROG)
	$(CP) $< $(RUNHASKELL_PROG)

MOSTLY_CLEAN_FILES += $(RUNHASKELL_PROG)

ifneq "$(NO_INSTALL_RUNHASKELL)" "YES"
INSTALL_PROGS += $(RUNHASKELL_PROG)
endif

binary-dist::
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/runghc
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/runghc/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/runghc/
	$(INSTALL_PROGRAM) $(RUNHASKELL_PROG) $(BIN_DIST_DIR)/utils/runghc/

include $(TOP)/mk/target.mk
