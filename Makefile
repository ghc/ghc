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

#
# Totally evil hack to make the setting of SUBDIRS be dependent
# on whether we do `make install' or not. Having a $(ifeq ... ) would
# be preferable..
CURRENT_TARGET = $(MAKECMDGOALS)
SUBDIRS = $(shell if (test x$(CURRENT_TARGET) = xinstall) ; then echo $(ProjectsToInstall); else echo $(ProjectsToBuild); fi)

ifeq (x$(CURRENT_TARGET),xbinary-dist)
   include $(shell echo $(Project) | tr A-Z a-z)/mk/config.mk
endif

#
# Files to include in fptools source distribution
#
SRC_DIST_DIRS += mk docs CONTRIB distrib $(ProjectsToBuild)
SRC_DIST_FILES += configure.in config.guess config.sub configure aclocal.m4 acconfig.h README INSTALL Makefile install-sh

#
# Making a binary distribution
#
# To make a particular binary distribution: 
# set $(Project) to the name of the project (currently Ghc or Happy).

BIN_DIST_TMPDIR=$(shell pwd)
BIN_DIST_NAME=fptools

#
# list of toplevel directories to include in binary distrib.
#
BIN_DIST_MAIN_DIR=$($(Project)MainDir)
BIN_DIST_DIRS=$($(Project)BinDistDirs)

binary-dist:: binary-dist-pre

BIN_DIST_TOP= distrib/Makefile-bin.in \
	      distrib/configure-bin.in \
	      README \
	      distrib/INSTALL \
	      $(BIN_DIST_MAIN_DIR)/ANNOUNCE \
	      $(BIN_DIST_MAIN_DIR)/PATCHLEVEL \
	      $(BIN_DIST_MAIN_DIR)/RELEASE \
	      glafp-utils/mkdirhier/mkdirhier \
	      install-sh \
	      config.guess \
	      config.sub   \
	      aclocal.m4

binary-dist::
	@for i in $(BIN_DIST_TOP); do \
	  if test -f "$$i"; then \
	     echo cp $$i $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME); \
	     cp $$i $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME); \
	  fi; \
	done;
	@echo "Configuring the Makefile for this project..."
	touch $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "package = $(ProjectNameShort)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "version = $(ProjectVersion)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_SH_SCRIPTS = $($(Project)BinDistShScripts)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_PRL_SCRIPTS = $($(Project)BinDistPrlScripts)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_LIB_PRL_SCRIPTS = $($(Project)BinDistLibPrlScripts)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_BINS = $($(Project)BinDistBins)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	cat $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile-bin.in >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	@echo "Generating a shippable configure script.."
	$(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/configure-bin.in $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/configure.in 
	( cd $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME); autoconf )

#
# binary dist'ing the documentation.  
# Which documentation to build/install is hardcoded below.
#

BINDIST_DOCS = $($(Project)BinDistDocs)
BINDIST_DOCS_WAYS = html info dvi

binary-dist ::
	@for way in $(BINDIST_DOCS_WAYS); do \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/$$way/$(ProjectNameShort)-$(ProjectVersion); \
	   for dir in $(BINDIST_DOCS); do \
	     echo Making $$way documentation in $$dir && \
	     $(MAKE) -C $$dir --no-print-directory $(MFLAGS) $$way && \
	     echo cp -f $$dir/*.$$way $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/$$way/$(ProjectNameShort)-$(ProjectVersion) && \
	     cp -f $$dir/*.$$way $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/$$way/$(ProjectNameShort)-$(ProjectVersion) && \
	     echo "Done."; \
	   done; \
	done

# Rename scripts to $i.prl and $i.sh where necessary.
# ToDo: do this in a cleaner way...

ifneq "$($(Project)BinDistPrlScripts)" ""
binary-dist::
	@for i in $($(Project)BinDistPrlScripts); do \
	     echo "Renaming $$i to $$i.prl"; \
	    $(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$(ProjectNameShort)-$(ProjectVersion)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$(ProjectNameShort)-$(ProjectVersion)/$$i.prl; \
	done
endif

ifneq "$($(Project)BinDistLibPrlScripts)" ""
binary-dist::
	@for i in $($(Project)BinDistLibPrlScripts); do \
	     echo "Renaming $$i to $$i.prl"; \
	    $(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$(ProjectNameShort)-$(ProjectVersion)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$(ProjectNameShort)-$(ProjectVersion)/$$i.prl; \
	done
endif

ifneq "$($(Project)BinDistShScripts)" ""
binary-dist::
	@for i in $($(Project)BinDistShScripts); do \
	     echo "Renaming $$i to $$i.sh"; \
	    $(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$(ProjectNameShort)-$(ProjectVersion)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$(ProjectNameShort)-$(ProjectVersion)/$$i.sh; \
	done
endif

dist :: dist-pre
include $(TOP)/mk/target.mk
dist :: dist-post

binary-dist::
	@echo "Mechanical and super-natty! Inspect the result and *if* happy; freeze, sell and get some sleep!"
