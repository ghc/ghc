# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.73 2002/02/12 15:17:17 simonmar Exp $
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
INSTALL_DATAS += package.conf ghc-usage.txt

CLEAN_FILES += package.conf.inplace package.conf

include $(TOP)/mk/target.mk
