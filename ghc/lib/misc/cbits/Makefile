#
# Makefile for cbits subdirectory
#
TOP=../../..
include $(TOP)/mk/boilerplate.mk
override WAYS=

CC:=$(HC)

C_SRCS=$(wildcard *.c)

# Remove Readline.lhs if readline.h isn't available.
ifneq "$(HAVE_READLINE)" "YES"
  C_SRCS := $(filter-out ghcReadline.c,$(C_SRCS))
endif

# ToDo: properly
# -D__GNUC__ : added to turn off noise from byteorder.h with 2.7.2 / Solaris-2.3
#
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR) -D__GNUC__
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR)

LIBRARY=libHSmisc_cbits.a
LIBOBJS=$(C_OBJS)
INSTALL_LIBS += $(LIBRARY)

include $(TOP)/mk/target.mk
