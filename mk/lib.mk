#-----------------------------------------------------------------------------
# $Id: lib.mk,v 1.3 1997/01/07 13:14:36 simonm Exp $

# Useful variables:

#	ARCHIVE =		archive name
# 	LIBOBJS = 		list of object files
#	DESTDIR =		where to install
#	INSTALLED_NAME =	name to install as (default = $(ARCHIVE))
#	C_DEP_SRCS =		sources for C make depend (optional)
#	HS_DEP_SRCS =		sources for Haskell make depend (optional)

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
	$(INSTALL) $(INSTLIBFLAGS) $(ARCHIVE) $(DESTDIR)/$(INSTALLED_NAME)
	cd $(DESTDIR); $(RANLIB) $(INSTALLED_NAME)

clean 	:: 
	$(RM) $(LIBOBJS)
	$(RM) $(ARCHIVE)

ifdef C_DEP_SRCS
include $(TOP)/mk/cdepend.mk
endif

ifdef HS_DEP_SRCS
include $(TOP)/mk/hsdepend.mk
endif
