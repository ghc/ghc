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
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SUBDIRS = std exts concurrent posix misc
else
SUBDIRS = std exts concurrent misc
endif
endif

include $(TOP)/mk/target.mk
