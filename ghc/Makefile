#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 1997/03/24 04:39:38 sof Exp $
#

TOP=.
include $(TOP)/mk/boilerplate.mk

#
# subdir dependencies:
#	everything needs utils
#	includes/ needs driver (to easily c.pile mkNativeGen.c)
#	make depend except in {utils,driver} needs includes	
#	RTS and compiler need includes
#

#
# Order is important! driver/ has to come before includes/ which
# again has to come before the rest.
#
# If we're booting from .hc files, swap the order
# we descend into compiler/ and lib/
#
ifeq "$(GhcWithHscBuiltViaC)" "NO"
SUBDIRS = utils driver includes runtime docs compiler lib
else
SUBDIRS = utils driver includes runtime docs lib compiler
endif

include $(TOP)/mk/target.mk
