TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG = $(GHC_GENAPPLY_PGM)

SRC_HC_OPTS += -I$(GHC_INCLUDE_DIR)

# genapply is needed to boot in ghc/rts...
ifneq "$(BootingFromHc)" "YES"
boot :: all
endif

include $(TOP)/mk/target.mk
