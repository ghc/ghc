TOP=..
include $(TOP)/mk/boilerplate.mk

C_SRCS=lndir.c
C_PROG=lndir

CLEAN_FILES += $(C_PROG)$(exeext) $(C_OBJS)
DESTDIR=$(INSTBINDIR)

include $(TOP)/mk/target.mk

CC_OPTS := $(filter-out -mno-cygwin -mwin32,$(CC_OPTS))