#-----------------------------------------------------------------------------
# $Id: clib.mk,v 1.2 1996/11/21 16:50:31 simonm Exp $

# Useful variables:

#	ARCHIVE =		archive name
# 	LIBOBJS = 		list of object files
#	DESTDIR =		where to install
#	INSTALLED_NAME =	name to install as (default = $(ARCHIVE))
#	DEP_SRCS =		sources for make depend (optional)

#-----------------------------------------------------------------------------

ifndef INSTALLED_NAME
INSTALLED_NAME	= $(ARCHIVE)
endif

all :: $(ARCHIVE)

$(ARCHIVE) :: $(LIBOBJS)
	@$(RM) $@
	$(AR) $@ $(LIBOBJS)
	$(RANLIB) $@

install	:: $(ARCHIVE)
	$(INSTALL) $(INSTDATAFLAGS) $(ARCHIVE) $(DESTDIR)/$(INSTALLED_NAME)
	cd $(DESTDIR)/$(INSTALLED_NAME); $(RANLIB) $(INSTALLED_NAME)

clean 	:: 
	$(RM) $(ARCHIVE)

ifdef DEP_SRCS
include $(TOP)/mk/cdepend.mk
endif
