#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 1997/03/19 20:44:51 simonpj Exp $
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
SUBDIRS = utils driver includes runtime compiler lib
else
SUBDIRS = utils driver includes runtime lib compiler
endif

include $(TOP)/mk/target.mk
