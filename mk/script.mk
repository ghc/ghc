#-----------------------------------------------------------------------------
# $Id: script.mk,v 1.2 1996/11/21 16:50:39 simonm Exp $

# Useful variables:

#	PROG =			program name
#	DEPLIST =		(optional) dependencies
# 	SRC =			source file
#	INTERP = 		(optional) interpretter
#	DESTDIR =		where to install
#	INSTALLED_NAME =	(optional) name to install as

#-----------------------------------------------------------------------------

ifndef INSTALLED_NAME
INSTALLED_NAME	= $(PROG)
endif

all 	:: $(PROG)

# Hack alert!  Since the variables msub needs aren't in the immediate
# Makefile, we must include the relevant files directly.

MSUB_OPTS = -f Makefile -f $(TOP)/mk/platform.mk -f $(TOP)/mk/utils.mk

$(PROG) ::  $(SRC) $(DEPLIST)
	$(RM) $@
ifdef INTERP
	echo "#!"$(INTERP) > $@
endif
	$(MSUB) $(MSUBFLAGS) INSTALLING=0 $(SRC) >> $@ \
		|| ( $(RM) $@ && exit 1 )
	chmod a+x $@

# aaargh: scripts sometimes do different things depending on whether they
# are installed versions or not (eg. use installed programs rather than
# those in the source tree) hence the following hack:

install	:: $(PROG)_tmp
	$(INSTALL) $(INSTSCRIPTFLAGS) $(PROG)_tmp $(DESTDIR)/$(INSTALLED_NAME)
	$(RM) $(PROG)_tmp

$(PROG)_tmp :  $(SRC)
	$(RM) $@
ifdef INTERP
	echo "#!"$(INTERP) > $@
endif
	$(MSUB) $(MSUBFLAGS) INSTALLING=1 $(SRC) >> $@ \
		|| ( $(RM) $@ && exit 1 )
	chmod a+x $@

clean 	:: 
	$(RM) $(PROG) $(PROG)_tmp
