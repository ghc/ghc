#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 1996/12/19 09:13:55 simonpj Exp $

TOP = ../..
include $(TOP)/ghc/mk/ghc.mk

SUBDIRS = cbits
ifeq ($(IncludeTestDirsInBuild),YES)
  SUBDIRS += tests
endif

include $(TOP)/mk/subdir.mk

# per-build options: shared with runtime system
include ../mk/buildflags.mk

#-----------------------------------------------------------------------------
# libHS.a, in several different 'ways'

all ::
	@for i in $(WAY_SUFFIXES); do \
		echo; \
		echo =========== Making libraries for way $$i; \
		echo; \
		$(MAKE) -f Makefile.libHS suffix=$$i; \
	done

# Shortcut for typical case when testing: just make the "normal" version
libHS.a ::
	$(MAKE)  -f Makefile.libHS suffix=norm

install ::
	@for i in $(WAY_SUFFIXES); do \
		$(MAKE) -f Makefile.libHS suffix=$$i install; \
	done

clean ::
	@for i in $(WAY_SUFFIXES); do \
		$(MAKE) -f Makefile.libHS suffix=$$i clean; \
	done

# Dependencies are done by Makefile.libHS

depend ::
	@$(MAKE) -f Makefile.libHS depend


# install MODULES file

install ::
	$(INSTALL) $(INSTDATAFLAGS) MODULES $(INSTDATADIR_GHC)/imports
