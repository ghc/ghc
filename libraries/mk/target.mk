# $Id: target.mk,v 1.12 2002/07/02 13:49:27 simonmar Exp $

TOP:=$(TOP)/..

# All the libs in here are "hierarchical", this flag tell the
# installation machinery to make sure that when installing interface
# files we maintain the directory structure.
HIERARCHICAL_LIB = YES

# NOT YET: Haddock needs to understand about .raw-hs files
#
# Set our source links to point to the CVS repository on the web.
# SRC_HADDOCK_OPTS += -s http://cvs.haskell.org/cgi-bin/cvsweb.cgi/fptools/libaries/$(PACKAGE)

include $(TOP)/mk/target.mk

TOP:=$(LIBRARIES_TOP)/..
