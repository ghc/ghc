#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1996/11/21 16:49:10 simonm Exp $

TOP=../../..
include $(TOP)/ghc/mk/ghc.mk

DYN_LOADABLE_BITS = \
	parse-gcstats.prl \
	process-gcstats.prl

PROG=stat2resid
SRC=stat2resid.prl
INTERP=$(PERL)
DESTDIR=$(INSTBINDIR_GHC)

install ::
	$(INSTALL) $(INSTDATAFLAGS) $(DYN_LOADABLE_BITS) $(INSTLIBDIR_GHC)

include $(TOP)/mk/script.mk
