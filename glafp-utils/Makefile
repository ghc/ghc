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
#
# ToDo: remove msub for good
#
SUBDIRS = mkdependC lndir ltx mkdirhier runstdtest sgmlverb

include $(TOP)/mk/target.mk
