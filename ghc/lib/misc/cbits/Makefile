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

SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR)

LIBRARY=libHSmisc_cbits.a
LIBOBJS=$(C_OBJS)
INSTALL_LIBS += $(LIBRARY)

include $(TOP)/mk/target.mk
