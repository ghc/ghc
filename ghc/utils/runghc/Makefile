TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG		= runghc
INSTALL_PROGS   += $(HS_PROG)

UseGhcForCc = YES
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

include $(TOP)/mk/target.mk
