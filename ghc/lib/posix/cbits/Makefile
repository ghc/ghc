#
# Makefile for cbits subdirectory
#
TOP=../../..
include $(TOP)/mk/boilerplate.mk
override WAYS=

# Hack!
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR)

CC=$(HC)
C_SRCS=$(wildcard *.c)
LIBRARY=libHSposix_cbits.a
LIBOBJS=$(C_OBJS)
INSTALL_LIBS += $(LIBRARY)

include $(TOP)/mk/target.mk
