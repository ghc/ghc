TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG		= runghc$(exeext)
INSTALL_PROGS   += $(HS_PROG)

UseGhcForCc = YES
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

# This causes libghccompat.a to be used:
include $(GHC_COMPAT_DIR)/compat.mk

# This is required because libghccompat.a must be built with
# $(GhcHcOpts) because it is linked to the compiler, and hence
# we must also build with $(GhcHcOpts) here:
SRC_HC_OPTS += $(GhcHcOpts) $(GhcStage1HcOpts)
SRC_HC_OPTS += -Wall

RUNHASKELL_PROG = runhaskell$(exeext)
all :: $(RUNHASKELL_PROG)

$(RUNHASKELL_PROG) : $(HS_PROG)
	$(CP) $< $(RUNHASKELL_PROG)

CLEAN_FILES += $(RUNHASKELL_PROG)

ifneq "$(NO_INSTALL_RUNHASKELL)" "YES"
INSTALL_PROGS += $(RUNHASKELL_PROG)
endif

binary-dist::
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/runghc
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/runghc/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/runghc/
	$(INSTALL_PROGRAM) $(RUNHASKELL_PROG) $(BIN_DIST_DIR)/utils/runghc/

include $(TOP)/mk/target.mk
