TOP=../..
include $(TOP)/mk/boilerplate.mk

C_PROG 	= hp2ps

SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -Wall

INSTALL_PROGS += $(C_PROG)

LIBS	     = $(LIBM)

CLEAN_FILES += $(C_OBJS) $(C_PROG)

include $(TOP)/mk/target.mk
