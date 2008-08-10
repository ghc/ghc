TOP=../..
include $(TOP)/mk/boilerplate.mk

SRC_CC_OPTS += $(MACOSX_DEPLOYMENT_CC_OPTS)
SRC_LD_OPTS += $(MACOSX_DEPLOYMENT_LD_OPTS)

C_PROG 	= hp2ps

SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -Wall

INSTALL_PROGS += $(C_PROG)

LIBS	     = $(LIBM)

CLEAN_FILES += $(C_OBJS) $(C_PROG)

BINDIST_EXTRAS += $(C_PROG)
include $(TOP)/mk/bindist.mk

include $(TOP)/mk/target.mk
