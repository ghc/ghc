#-----------------------------------------------------------------------------
# $Id: HSprog.mk,v 1.2 1996/11/21 16:50:28 simonm Exp $

# Useful variables:

#	PROG =			program name
# 	SRCS =			list of source files
# 	LIBS =			libraries
#	DEPLIBS =		more libraries (may depend on $(LIBS)
#	DESTDIR =		where to install
#	INSTALLED_NAME =	name to install as (default = $(PROG))

#-----------------------------------------------------------------------------

ifndef INSTALLED_NAME
INSTALLED_NAME = $(PROG)
endif

OBJS = $($(SRCS:.hs.o):.lhs.o)

all 	:: $(PROG)

$(PROG) :: $(OBJS)
	$(HC) -o $@ $(HCFLAGS) $(LDOPTIONS) $(OBJS) $(DEPLIBS) $(LIBS)

install :: $(PROG)
	$(INSTALL) $(INSTBINFLAGS) $(PROG) $(DESTDIR)/$(INSTALLED_NAME)

clean 	::
	$(RM) $(PROG)

tags 	:: $(SRCS)
	$(HSTAGS) $(HSTAGSFLAGS) $(SRCS)

ifndef OmitHSDepend
  HS_DEP_SRCS = $(SRCS)
  include $(TOP)/mk/hsdepend.mk
endif
