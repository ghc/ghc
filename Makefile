#################################################################################
#
#			fptools/Makefile
#
#		This is the main Makefile for fptools.
#
#################################################################################

TOP=.
include $(TOP)/mk/boilerplate.mk
SRC_DIST_DIR=$(shell pwd)/$(SRC_DIST_NAME)

SUBDIRS = $(ProjectsToBuild)

#
# Files to include in fptools source distribution
#
SRC_DIST_DIRS += mk $(ProjectsToBuild)
SRC_DIST_FILES += configure.in config.guess config.sub configure README ANNOUNCE NEWS INSTALL Makefile


#
# Making a binary distribution
#
BIN_DIST_TMPDIR=$(shell pwd)
BIN_DIST_NAME=fptools

#
# list of toplevel `projects' to include in binary distrib.
#
BIN_DIST_DIRS=ghc

binary-dist:: binary-dist-pre

BIN_DIST_TOP= distrib/Makefile-bin.in \
	      distrib/configure-bin.in \
	      distrib/README \
	      distrib/INSTALL \
	      distrib/ANNOUNCE

binary-dist::
	@for i in $(BIN_DIST_TOP); do \
	  @echo cp $$i $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
	  cp $$i $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
	done;

#
# Creating and copying the documentation into the bin-dist tree.
#
binary-dist::
	$(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/html
	$(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/dvi
	$(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/info
	@echo "Making html documentation.."
	$(MAKE) -C docs --no-print-directory $(MFLAGS) html
	cp docs/html/* $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/html
	@echo "Making dvi files.."
	$(MAKE) -C docs --no-print-directory $(MFLAGS) dvi
	cp docs/*.dvi  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/dvi
	@echo "Making info files.."
	$(MAKE) -C docs --no-print-directory $(MFLAGS) info
	cp docs/*.info* $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/info

dist :: dist-pre
include $(TOP)/mk/target.mk
dist :: dist-post

#
# Automatically remake update configuration files
# (from autoconf manual)
#
configure: configure.in
	autoconf
#     
# autoheader might not change config.h.in, so touch a stamp file.
#
mk/config.h.in: mk/stamp-h.in
mk/stamp-h.in: configure.in
	autoheader
	echo timestamp > mk/stamp-h.in

mk/config.h: mk/stamp-h
mk/stamp-h: mk/config.h.in config.status
	./config.status
     
config.status: configure
	./config.status --recheck

.PHONY: config

config: config.status
	@:
