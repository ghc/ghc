#-----------------------------------------------------------------------------
# $Id: ways.mk,v 1.1 1997/01/07 13:16:54 simonm Exp $

# Build an object in several different ways, using a subsidiary Makefile.

#	MAKEFILE = 	The Makefile to invoke for each way
#	DESCR    =	Description of object being built

ifndef NoWayAllTarget
all ::
	@for i in $(WAY_SUFFIXES); do \
		echo; \
		echo =========== Making $(DESCR) for way $$i; \
		echo; \
		$(MAKE) -f $(MAKEFILE) suffix=$$i; \
	done
endif

ifndef NoWayInstallTarget
install ::
	@for i in $(WAY_SUFFIXES); do \
		$(MAKE) -f $(MAKEFILE) suffix=$$i install; \
	done
endif

ifndef NoWayCleanTarget
clean ::
	@for i in $(WAY_SUFFIXES); do \
		$(MAKE) -f $(MAKEFILE) suffix=$$i clean; \
	done
endif

# We normally only want to make dependencies once

ifndef NoWayDependTarget
depend ::
	@$(MAKE) -f $(MAKEFILE) depend
endif
