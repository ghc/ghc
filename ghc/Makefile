#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.24 2004/02/12 02:11:34 mthomas Exp $
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
# we descend into subdirs - to boot utils must be before driver.
#

# First - check whether we should build the docs directory.
# On Windows, the tools to build docs are not always available.

ifneq "$(SGMLDocWays)" ""
DOCS_DIR = docs
else
DOCS_DIR =
endif

ifeq "$(BootingFromHc)" "YES"
SUBDIRS = includes rts $(DOCS_DIR) compiler utils driver
else
ifneq "$(ILXized)" "YES"
SUBDIRS = includes utils driver $(DOCS_DIR) compiler rts
else
# No RTS for ILX
SUBDIRS = includes utils driver $(DOCS_DIR) compiler
endif
endif

ifeq ($(IncludeTestDirsInBuild),YES)
SUBDIRS += tests
endif

SRC_DIST_FILES += configure

DIST_CLEAN_FILES += config.cache config.status ghc.spec

extraclean::
	$(RM) -rf autom4te.cache

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
# These files need to be in the InstallShield
# INSTALL_DATAS rather than INSTALL_DOCS is used so these files go
# in the top-level directory of the distribution
INSTALL_DATAS += ANNOUNCE LICENSE README VERSION
endif

include $(TOP)/mk/target.mk
