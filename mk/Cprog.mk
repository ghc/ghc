#-----------------------------------------------------------------------------
# $Id: Cprog.mk,v 1.2 1996/11/21 16:50:26 simonm Exp $

# Useful variables:

#	PROG =			program name
# 	SRCS =			list of sources (optional for one file)
# 	LIBS =			libraries
#	DEPLIBS =		more libraries (may depend on $(LIBS)
#	DESTDIR =		where to install
#	INSTALLED_NAME =	name to install as (default = $(PROG))

#-----------------------------------------------------------------------------

ifndef INSTALLED_NAME
INSTALLED_NAME	= $(PROG)
endif

ifndef SRCS
SRCS = $(PROG).c
endif

OBJS = $(SRCS:.c=.o)

all 	:: $(PROG)

$(PROG) :: $(OBJS)
	$(CC) -o $@ $(CFLAGS) $(LDOPTIONS) $(OBJS) $(DEPLIBS) $(LIBS)

install	:: $(PROG)
	$(INSTALL) $(INSTBINFLAGS) $(PROG) $(DESTDIR)/$(INSTALLED_NAME)

clean 	:: 
	$(RM) $(PROG)

tags	::
	$(CTAGS) -a $(CTAGSFLAGS) $(SRCS)

ifndef OmitCDepend
  C_DEP_SRCS = $(SRCS)
  include $(TOP)/mk/cdepend.mk
endif
