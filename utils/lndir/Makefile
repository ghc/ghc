TOP=../..
include $(TOP)/mk/boilerplate.mk

C_SRCS=lndir.c
C_PROG=lndir

CLEAN_FILES += $(C_PROG)$(exeext) $(C_OBJS)
DESTDIR=$(INSTBINDIR)

include $(TOP)/mk/target.mk

# Win32: cygwin supports symlinks, so make sure we
#        don't feed in the mingw flags here. In other
#        words, lndir.exe is a cygwin-based app.
CC_OPTS := $(filter-out -mno-cygwin,$(CC_OPTS))
