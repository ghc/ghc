TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG		= runghc
INSTALL_PROGS   += $(HS_PROG)

UseGhcForCc = YES
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

SRC_HC_OPTS += -i$(GHC_LIB_COMPAT_DIR)
SRC_LD_OPTS += -L$(GHC_LIB_COMPAT_DIR) -lghccompat

include $(TOP)/mk/target.mk
