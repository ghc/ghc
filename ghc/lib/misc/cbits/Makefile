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
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_LIB_DIR)/std/cbits

LIBRARY=libHSmisc_cbits.a
LIBOBJS=$(C_OBJS)
INSTALL_LIBS += $(LIBRARY)

DLL_NAME = HSmisc_cbits.dll
SRC_BLD_DLL_OPTS += --export-all --output-def=HSmisc_cbits.def
SRC_BLD_DLL_OPTS += -lwinmm -lwsock32 -lHSrts_imp -lHS_cbits_imp -lgmp -L. -L../../../rts/gmp -L../../../rts -L../../std/cbits

include $(TOP)/mk/target.mk
