#################################################################################
#
#			    nofib/Makefile
#
#		Toplevel Makefile for the nofib project
#
# 		$Id: Makefile,v 1.6 1998/04/14 10:38:57 simonm Exp $
#
#################################################################################

TOP = .
include $(TOP)/mk/boilerplate.mk

# Set up which parts of the nofib suite that is to be
# run. See $(TOP)/../mk/config.mk, which tells you how
# to set NoFibSubDirs
#
# As usual,if you want to override these, create
# $(TOP)/../mk/build.mk containing the flags and options
# you want to use in a build tree.
SUBDIRS = runstdtest nofib-analyse $(NoFibSubDirs)

# Include the standard targets, one of which
# causes make to descend into the SUBDIRS.

#
# Creating a nofib distribution
#
SRC_DIST_DIR=$(shell pwd)/nofib
SRC_DIST_NAME=nofib
SRC_DIST_DIRS=docs fibon gc imaginary smp spectral real parallel mk shootout

dist :: nofib-dist-pre
include $(TOP)/mk/target.mk
dist :: dist-post
dist :: dist-package

