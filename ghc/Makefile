#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.30 2005/03/02 14:35:05 simonmar Exp $
#

TOP=.
include $(TOP)/mk/boilerplate.mk

#
# subdir dependencies:
#	everything needs utils
#	utils needs lib
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
SUBDIRS = includes rts docs compiler lib utils driver
else
ifneq "$(ILXized)" "YES"
SUBDIRS = includes lib utils driver docs compiler rts
else
# No RTS for ILX
SUBDIRS = includes lib utils driver docs compiler
endif
endif

SRC_DIST_FILES += configure

# Clean everything created by configure:
DIST_CLEAN_FILES += VERSION config.cache config.status ghc.spec \
	docs/users_guide/ug-book.xml

# don't clean version.mk: it's needed when cleaning stuff later on
LATE_DIST_CLEAN_FILES += mk/config.mk 

extraclean::
	$(RM) -rf autom4te.cache

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
# These files need to be in the InstallShield
# INSTALL_DATAS rather than INSTALL_DOCS is used so these files go
# in the top-level directory of the distribution
INSTALL_DATAS += ANNOUNCE LICENSE README VERSION
endif

include $(TOP)/mk/target.mk
