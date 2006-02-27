TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG = $(GHC_GENAPPLY_PGM)

# genapply is needed to boot in ghc/rts...
ifneq "$(BootingFromHc)" "YES"
boot :: all
endif

ifeq "$(ghc_ge_504)" "NO"
SRC_HC_OPTS +=  -package lang -package util -package text
endif

ifeq "$(GhcUnregisterised)" "YES"
SRC_HC_OPTS += -DNO_REGS
endif

# Try to get dependencies right...
SRC_HC_OPTS += -no-recomp
GenApply.o : $(GHC_INCLUDE_DIR)/ghcconfig.h
GenApply.o : $(GHC_INCLUDE_DIR)/MachRegs.h
GenApply.o : $(GHC_INCLUDE_DIR)/Constants.h

include $(TOP)/mk/target.mk
