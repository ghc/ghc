#-----------------------------------------------------------------------------
# $Id: target.mk,v 1.1 2002/04/04 16:23:42 simonmar Exp $
# target.mk project stub
#

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
HADDOCK_TOP := $(TOP)
TOP:=$(TOP)/..

include $(TOP)/mk/target.mk

HADDOCK_INPLACE = $(HADDOCK_TOP)/src/haddock-inplace

# Reset TOP
TOP:=$(HADDOCK_TOP)
