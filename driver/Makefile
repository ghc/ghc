# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.76 2005/03/02 09:49:11 simonmar Exp $
#
# (c) The University of Glasgow 2002
#

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = mangler split ghc ghc-pkg ghci runhaskell

INPLACE_PKG_CONF = $(INPLACE_DATA_DIR)/package.conf
# Used to signal that we are inplace, as opposed to installed:
INPLACE_FILE     = $(INPLACE_DATA_DIR)/inplace

boot all ::
	$(MKDIRHIER) $(INPLACE_DATA_DIR)
	test -f $(INPLACE_PKG_CONF) || echo "[]" > $(INPLACE_PKG_CONF)
	touch $(INPLACE_FILE)

INSTALL_DATAS += ghc-usage.txt ghci-usage.txt

# Since cleaning effectively uninstalls all the packages, we must 
# remove the stamp files that the build system uses to avoid unnecessarily
# re-installing the RTS
CLEAN_FILES += stamp-pkg-conf-rts

binary-dist:
ifeq "$(WHERE_AM_I)" ""
	echo "I don't know where I am" >&2
	exit 1
endif
	for FILE in Makefile $(INSTALL_DATAS); do echo $(WHERE_AM_I)/$$FILE >> $(BIN_DIST_LIST); done
	set -e; for d in $(SUBDIRS); do $(MAKE) -C $$d binary-dist WHERE_AM_I=$(WHERE_AM_I)/$$d; done

include $(TOP)/mk/target.mk
