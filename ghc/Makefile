#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.11 2000/06/25 17:18:15 panne Exp $
#

TOP=.
include $(TOP)/mk/boilerplate.mk

#
# subdir dependencies:
#	everything needs utils
#	includes/ needs driver (to easily c.pile mkNativeGen.c)
#	make depend except in {utils,driver} needs includes	
#	RTS and compiler need includes
#

#
# Order is important! It's e.g. necessary to descend into include/
# before the rest to have a config.h, etc.
#
# If we're booting from .hc files, swap the order
# we descend into compiler/ and lib/
#
ifeq "$(GhcWithHscBuiltViaC)" "NO"
SUBDIRS = includes utils driver rts docs compiler lib
else
SUBDIRS = includes utils driver rts docs lib compiler
endif

# Easier to copy
ifeq "$(BIN_DIST)" "1"
SUBDIRS := $(filter-out docs,$(SUBDIRS))
endif

ifeq ($(IncludeTestDirsInBuild),YES)
SUBDIRS += tests
endif

SRC_DIST_FILES += configure

include $(TOP)/mk/target.mk
