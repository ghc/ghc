#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.16 2001/04/24 13:24:45 simonmar Exp $
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
# we descend into compiler/ and lib/.
#
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = includes utils rts docs lib compiler driver
else
SUBDIRS = includes utils driver docs compiler rts lib
endif

# Easier to copy
ifeq "$(BIN_DIST)" "1"
SUBDIRS := $(filter-out docs,$(SUBDIRS))
endif

ifeq ($(IncludeTestDirsInBuild),YES)
SUBDIRS += tests
endif

SRC_DIST_FILES += configure

DIST_CLEAN_FILES += ghc.spec config.status
MAINTAINER_CLEAN_FILES += configure

include $(TOP)/mk/target.mk
