# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.76 2005/03/02 09:49:11 simonmar Exp $
#
# (c) The University of Glasgow 2002
#

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = mangler split ghc ghci

INPLACE_DATA_DIR = $(FPTOOLS_TOP_ABS)/inplace-datadir
INPLACE_PKG_CONF = $(INPLACE_DATA_DIR)/package.conf
# Used to signal that we are inplace, as opposed to installed:
INPLACE_FILE     = $(INPLACE_DATA_DIR)/inplace

boot all ::
	$(MKDIRHIER) $(INPLACE_DATA_DIR)
	test -e $(INPLACE_PKG_CONF) || echo "[]" > $(INPLACE_PKG_CONF)
	echo "[]" > package.conf
	touch $(INPLACE_FILE)

INSTALL_DATAS += package.conf ghc-usage.txt ghci-usage.txt

# Since cleaning effectively uninstalls all the packages, we must 
# remove the stamp files that the build system uses to avoid unnecessarily
# re-installing packages.
CLEAN_FILES += package.conf* stamp-pkg-conf*

binary-dist:
	$(INSTALL_DIR)                   $(BIN_DIST_DIR)/driver
	$(INSTALL_DATA) Makefile         $(BIN_DIST_DIR)/driver
	$(INSTALL_DATA) $(INSTALL_DATAS) $(BIN_DIST_DIR)/driver
	set -e; for d in $(SUBDIRS); do $(MAKE) -C $$d binary-dist; done

include $(TOP)/mk/target.mk
