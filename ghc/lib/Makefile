#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 1997/01/07 13:19:38 simonm Exp $

TOP = ../..
include $(TOP)/ghc/mk/ghc.mk

SUBDIRS = cbits
ifeq ($(IncludeTestDirsInBuild),YES)
  SUBDIRS += tests
endif
include $(TOP)/mk/subdir.mk

# per-build options: shared with runtime system
include ../mk/buildflags.mk

MAKEFILE=Makefile.libHS
DESCR=libraries
include $(TOP)/ghc/mk/ways.mk

# Shortcut for typical case when testing: just make the "normal" version
# (simonm Todo: better way to do this?)
libHS.a ::
	$(MAKE)  -f Makefile.libHS suffix=norm

# install MODULES file

install ::
	$(INSTALL) $(INSTDATAFLAGS) MODULES $(INSTDATADIR_GHC)/imports
