#################################################################################
#
#			    ghc/lib/Makefile
#
#		Makefile for building the GHC Prelude libraries umpteen ways
#
#	
#################################################################################

TOP = ..
include $(TOP)/mk/boilerplate.mk

# posix must be before misc.

ifeq "$(GhcWithHscBuiltViaC)" "YES"
SUBDIRS = std exts
else
SUBDIRS = std exts misc posix concurrent
endif

include $(TOP)/mk/target.mk
