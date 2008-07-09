TOP=../..
include $(TOP)/mk/boilerplate.mk

# Exclude for booting
ifeq "$(stage)" "2"
SRC_CC_OPTS += $(MACOSX_DEPLOYMENT_CC_OPTS)
SRC_LD_OPTS += $(MACOSX_DEPLOYMENT_LD_OPTS)
endif

C_SRCS=lndir.c
C_PROG=lndir

CLEAN_FILES += $(C_PROG)$(exeext) $(C_OBJS)
DESTDIR=$(INSTBINDIR)

include $(TOP)/mk/target.mk

