############################################################################
#
#			fptools/Makefile
#
#		This is the main Makefile for fptools.
#
############################################################################

TOP=.
include $(TOP)/mk/boilerplate.mk

#
# Totally evil hack to make the setting of SUBDIRS be dependent
# on whether we do `make install' or not. Having a $(ifeq ... ) would
# be preferable..
CURRENT_TARGET = $(MAKECMDGOALS)
SUBDIRS = $(shell if (test x$(CURRENT_TARGET) = xinstall) ; then echo $(ProjectsToInstall); else echo $(ProjectsToBuild); fi)

ifneq "$(Project)" ""
   include $(shell echo $(Project) | tr A-Z a-z)/mk/config.mk
endif

project-check :
	@if [ "$(Project)" = "" ]; then \
		echo "	You need to set \"Project\" in order to make $(CURRENT_TARGET)"; \
		echo "	eg. make $(CURRENT_TARGET) Project=Ghc"; \
		exit 1; \
	fi

# -----------------------------------------------------------------------------
# Make sure configure is up-to-date

all boot :: configure
configure :: configure.in
	@echo "WARNING: configure needs to be regenerated.  Type"
	@echo "      make -f Makefile.config ./configure"
	@echo "and rerun make."
	@exit 16

# -----------------------------------------------------------------------------
# Making a binary distribution
#
# To make a particular binary distribution: 
# set $(Project) to the name of the project, capitalised (eg. Ghc or Happy).

# `dist' `binary-dist'
#      Create a distribution tar file for this program. The tar file
#      should be set up so that the file names in the tar file start with
#      a subdirectory name which is the name of the package it is a
#      distribution for. This name can include the version number.
#
#      For example, the distribution tar file of GCC version 1.40 unpacks
#      into a subdirectory named `gcc-1.40'.
# 
#      The easiest way to do this is to create a subdirectory
#      appropriately named, use ln or cp to install the proper files in
#      it, and then tar that subdirectory.
# 
#      The dist target should explicitly depend on all non-source files
#      that are in the distribution, to make sure they are up to date in
#      the distribution. See Making Releases.
#
#	binary-dist is an FPtools addition for binary distributions
# 

binary-dist :: project-check

BIN_DIST_TMPDIR=$(FPTOOLS_TOP_ABS)
BIN_DIST_NAME=$(ProjectNameShort)-$(ProjectVersion)

#
# list of toplevel directories to include in binary distrib.
#
BIN_DIST_MAIN_DIR=$($(Project)MainDir)
BIN_DIST_DIRS=$($(Project)BinDistDirs)

binary-dist:: binary-dist-pre

BIN_DIST_TOP= distrib/Makefile-bin.in \
	      distrib/configure-bin.in \
	      distrib/INSTALL \
	      $(BIN_DIST_MAIN_DIR)/ANNOUNCE \
	      $(BIN_DIST_MAIN_DIR)/VERSION \
	      $(BIN_DIST_MAIN_DIR)/RELEASE \
	      $(BIN_DIST_MAIN_DIR)/LICENSE \
	      $(BIN_DIST_MAIN_DIR)/README \
	      glafp-utils/mkdirhier/mkdirhier \
	      install-sh \
	      config.guess \
	      config.sub   \
	      aclocal.m4

#
# binary-dist creates a binary bundle, set BIN_DIST_NAME
# to package name and do `make binary-dist Project=<project-name>'
# (normally this just a thing you would do from the toplevel of fptools)
#
.PHONY: binary-dist-pre binary-dist binary-pack

BIN_DIST_NAME=$(ProjectNameShort)-$(ProjectVersion)
BIN_DIST_TMPDIR=$(FPTOOLS_TOP_ABS)

binary-dist-pre::
	-rm -rf $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)
	-rm -f $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME).tar.gz
	@for i in $(BIN_DIST_DIRS); do 		 	 \
	  if test -d "$$i"; then 			 \
	   echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM); \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM); \
	   echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM); \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM); \
	   echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share; \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share; \
	   echo $(MAKE) -C $$i $(MFLAGS) install \
		prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		exec_prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		bindir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM) \
		libdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM) \
		libexecdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM) \
		datadir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share; \
	   $(MAKE) -C $$i $(MFLAGS) install \
		prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		exec_prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		bindir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM) \
		libdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM) \
		libexecdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM) \
		datadir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share; \
	  fi; \
	done

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
BINDIST_DOCS_WAYS = html ps

binary-dist ::
	@for way in $(BINDIST_DOCS_WAYS); do \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/$$way; \
	   for dir in $(BINDIST_DOCS); do \
	     echo Making $$way documentation in $$dir && \
	     $(MAKE) -C $$dir --no-print-directory $(MFLAGS) $$way >.doclog  2>&1 && \
	     if [ "$$way" = "html" ]; then \
		for subdir in `perl -n -e '/output will be in ([_\-A-Za-z0-9]*)/ && do { print "$$1 "; };' <.doclog`; do \
		   echo Copying HTML docs from $$subdir...; \
		   cp -Rf $$dir/$$subdir $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/$$way; \
		done \
	     else \
	        cp -f $$dir/*.$$way $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/$$way; \
	     fi && \
	     echo "Done."; \
	   done; \
	done
	@rm -f .doclog

# Rename scripts to $i.prl and $i.sh where necessary.
# ToDo: do this in a cleaner way...

ifneq "$($(Project)BinDistPrlScripts)" ""
binary-dist::
	@for i in $($(Project)BinDistPrlScripts); do \
	     echo "Renaming $$i to $$i.prl"; \
	    $(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$$i.prl; \
	done
endif

ifneq "$($(Project)BinDistLibPrlScripts)" ""
binary-dist::
	@for i in $($(Project)BinDistLibPrlScripts); do \
	     echo "Renaming $$i to $$i.prl"; \
	    $(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$$i.prl; \
	done
endif

ifneq "$($(Project)BinDistShScripts)" ""
binary-dist::
	@for i in $($(Project)BinDistShScripts); do \
	     echo "Renaming $$i to $$i.sh"; \
	    $(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$$i.sh; \
	done
endif

#
# Do this separately for now
# 
binary-pack::
	( cd $(BIN_DIST_TMPDIR); $(TAR) chzf $(BIN_DIST_NAME).tar.gz $(BIN_DIST_NAME) )

ifneq "$(way)" ""
package-way-dist::
	( cd $(BIN_DIST_TMPDIR); find $(BIN_DIST_NAME)/ \( -name "*$(_way).a" -o -name "*.$(way_)hi" \) -print | xargs tar cvf $(BIN_DIST_TMPDIR)/ghc-$(ProjectVersion)-$(way)-$(TARGETPLATFORM).tar )
	gzip $(BIN_DIST_TMPDIR)/ghc-$(ProjectVersion)-$(way)-$(TARGETPLATFORM).tar
endif

ifneq "$(way)" ""
remove-way-dist::
	( cd $(BIN_DIST_TMPDIR); find $(BIN_DIST_NAME)/ \( -name "*$(_way).a" -o -name "*.$(way_)hi" \) -print -exec rm -f {} \; )
endif

binary-dist::
	@echo "Mechanical and super-natty! Inspect the result and *if* happy; freeze, sell and get some sleep!"

# -----------------------------------------------------------------------------
# Building source distributions
#
# Do it like this: 
#
#	$ make boot
#	$ make dist Project=Ghc

.PHONY: dist

#
# Directory in which we're going to build the src dist
#
SRC_DIST_DIR=$(shell pwd)/$(SRC_DIST_NAME)

#
# Files to include in source distributions
#
SRC_DIST_DIRS += docs distrib $(ProjectsToBuild)
SRC_DIST_FILES += \
	configure.in config.guess config.sub configure \
	aclocal.m4 acconfig.h README Makefile install-sh \
	mk/boilerplate.mk mk/config.h.in mk/config.mk.in mk/opts.mk \
	mk/paths.mk mk/suffix.mk mk/target.mk

dist dist-manifest dist-package :: project-check

# clean the tree first, leaving certain extra files in place (eg. configure)
dist :: distclean

dist ::
	-rm -rf $(SRC_DIST_DIR)
	-rm -f $(SRC_DIST_NAME).tar.gz
	mkdir $(SRC_DIST_DIR)
	mkdir $(SRC_DIST_DIR)/mk
	( cd $(FPTOOLS_TOP_ABS); $(FIND) $(SRC_DIST_DIRS) -type d \( -name CVS -prune -o -name SRC -prune -o -name tests -prune -o -exec mkdir $(SRC_DIST_DIR)/{} \; \) ; )
	( cd $(FPTOOLS_TOP_ABS); $(FIND) $(SRC_DIST_DIRS) $(SRC_DIST_FILES) -name CVS -prune -o -name SRC -prune -o -name tests -prune -o -name "*~" -prune -o -name ".cvsignore" -prune -o -name "\#*" -prune -o -name ".\#*" -prune -o -name "log" -prune -o -name "*-SAVE" -prune -o -name "*.orig" -prune -o -name "*.rej" -prune -o ! -type d -exec $(LN_S) $(FPTOOLS_TOP_ABS)/{} $(SRC_DIST_DIR)/{} \; )

# Automatic generation of a MANIFEST file for a source distribution
# tree that is ready to go.
dist-manifest ::
	cd $(SRC_DIST_DIR); $(FIND) . \( -type l -o -type f \) -exec ls -lLG {} \; | sed -e 's/\.\///' > /tmp/MANIFEST ; mv /tmp/MANIFEST MANIFEST

dist-package :: dist-package-tar-gz

SRC_DIST_PATHS = $(patsubst %, $(SRC_DIST_NAME)/%, $(SRC_DIST_FILES) $(SRC_DIST_DIRS))

dist-package-tar-gz ::
	$(TAR) chzf $(SRC_DIST_NAME)-src.tar.gz $(SRC_DIST_NAME)

dist-package-zip ::
	cd ..; $(LN_S) $(FPTOOLS_TOP_ABS) $(SRC_DIST_NAME) && \
	       $(ZIP) $(ZIP_OPTS) -r $(SRC_DIST_NAME)-src.zip $(SRC_DIST_PATHS)

# -----------------------------------------------------------------------------

DIST_CLEAN_FILES += config.cache config.status

MAINTAINER_CLEAN_FILES += configure

include $(TOP)/mk/target.mk

