#################################################################################
#
#			    glafp-utils/Makefile
#
#		Main Makefile for project glafp-utils
#
#################################################################################

TOP=.
include $(TOP)/mk/boilerplate.mk

# We need to write mkdependC first (in that order), to be sure that
# make depend will succeed in all the other directories.
SUBDIRS = mkdependC mkdirhier runstdtest docbook
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SUBDIRS += lndir
endif

include $(TOP)/mk/target.mk
