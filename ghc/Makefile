#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.22 2003/03/31 12:23:41 simonmar Exp $
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
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = includes rts docs compiler utils driver
else
ifneq "$(ILXized)" "YES"
SUBDIRS = includes utils driver docs compiler rts
else
# No RTS for ILX
SUBDIRS = includes utils driver docs compiler
endif
endif

ifeq ($(IncludeTestDirsInBuild),YES)
SUBDIRS += tests
endif

SRC_DIST_FILES += configure

DIST_CLEAN_FILES += ghc.spec config.status

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
# These files need to be in the InstallShield
# INSTALL_DATAS rather than INSTALL_DOCS is used so these files go
# in the top-level directory of the distribution
INSTALL_DATAS += ANNOUNCE LICENSE README VERSION
endif

include $(TOP)/mk/target.mk
