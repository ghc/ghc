# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.75 2003/11/06 10:31:55 simonmar Exp $
#
# (c) The University of Glasgow 2002
#

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = mangler split ghc ghci

boot all :: package.conf.inplace package.conf

package.conf.inplace :
	echo "[]" > $@

package.conf :
	echo "[]" > $@

override datadir = $(libdir)
INSTALL_DATAS += package.conf ghc-usage.txt ghci-usage.txt

# Since cleaning effectively uninstalls all the packages, we must 
# remove the stamp files that the build system uses to avoid unnecessarily
# re-installing packages.
CLEAN_FILES += package.conf.inplace package.conf stamp-pkg-conf*

include $(TOP)/mk/target.mk
