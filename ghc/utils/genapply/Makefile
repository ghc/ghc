TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG = $(GHC_GENAPPLY_PGM)

# genapply is needed to boot in ghc/rts...
ifneq "$(BootingFromHc)" "YES"
boot :: all
endif

ghc_ge_504 = $(shell if (test $(GhcCanonVersion) -ge 504); then echo YES; else echo NO; fi)

ifeq "$(ghc_ge_504)" "NO"
SRC_HC_OPTS +=  -package lang -package util -package text
endif

ifeq "$(GhcUnregisterised)" "YES"
SRC_HC_OPTS += -DNO_REGS
endif

include $(TOP)/mk/target.mk
