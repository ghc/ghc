############################################################################
#
#		This is the top-level Makefile for GHC
#
# Targets: 
#
# 	bootstrap (DEFAULT)
#		Builds GHC, then builds the libraries,
#		then uses this GHC ("stage 1") to build itself
#		("stage 2").
#
#	bootstrap2
#		Same as bootstrap
#
#	bootstrap3
#		bootstrap2 + we build GHC one more time ("stage 3")
#
#	stage1
#		Just build up to stage 1
#
#	stage2
#		Just build stage 2 (stage 1 must be built)
#
#	stage3
#		Just build stage 3 (stage 2 must be built)
#
#	all
#		Same as bootstrap
#
#       install
#		Install everything, including stage 2 compiler by default
#		(override with stage=3, for example).
#
#	dist
#		Make a source dist (WARNING: runs 'make distclean' first)
#
#	binary-dist
#		Builds a binary distribution
#
#	hc-file-bundle
#		Builds an HC-file bundle, for bootstrapping
#
#	clean, distclean, maintainer-clean
#		Increasing levels of cleanliness
#
############################################################################

TOP=.
include $(TOP)/mk/boilerplate.mk

#
# Order is important! It's e.g. necessary to descend into include/
# before the rest to have a config.h, etc.
#
# If we're booting from .hc files, swap the order
# we descend into subdirs - to boot utils must be before driver.
#
.PHONY: stage1 stage2 stage3 bootstrap bootstrap2 bootstrap3

# We can't 'make boot' in libraries until stage1 is built
ifeq "$(BootingFromHc)" "YES"
SUBDIRS_NOLIB = includes rts compat compiler docs utils driver
else
SUBDIRS_NOLIB = includes compat utils driver docs compiler rts
endif

SUBDIRS = $(SUBDIRS_NOLIB) libraries

VERSION :
	echo $(ProjectVersion) >VERSION

all :: VERSION

# Sanity check that all the core libraries are in the tree, to catch
# failure to run darcs-all.
check-packages :
	@for d in `cat libraries/core-packages`; do \
	  if test ! -d libraries/$$d; then \
	     echo "Looks like you're missing libraries/$$d,"; \
	     echo "maybe you haven't done 'sh darcs-all get'?"; \
	     exit 1; \
	  fi \
	done

stage1 : check-packages
	$(MAKE) -C utils/mkdependC boot
	@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \
	for i in $(SUBDIRS_NOLIB); do \
	  echo "------------------------------------------------------------------------"; \
	  echo "== $(MAKE) boot $(MFLAGS);"; \
	  echo " in $(shell pwd)/$$i"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) --no-print-directory -C $$i $(MFLAGS) boot; \
	  if [ $$? -eq 0 -o $$x_on_err -eq 0 ] ;  then true; else exit 1; fi; \
	done; \
	for i in $(SUBDIRS_NOLIB); do \
	  echo "------------------------------------------------------------------------"; \
	  echo "== $(MAKE) all $(MFLAGS);"; \
	  echo " in $(shell pwd)/$$i"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) --no-print-directory -C $$i $(MFLAGS) all; \
	  if [ $$? -eq 0 -o $$x_on_err -eq 0 ] ;  then true; else exit 1; fi; \
	done
	@$(MAKE) -C libraries boot
	@$(MAKE) -C libraries all

stage2 : check-packages
	$(MAKE) -C compiler boot stage=2
	$(MAKE) -C compiler stage=2

stage3 : check-packages
	$(MAKE) -C compiler boot stage=3
	$(MAKE) -C compiler stage=3

bootstrap  : bootstrap2

bootstrap2 : stage1
	$(MAKE) stage2

bootstrap3 : bootstrap2
	$(MAKE) stage3

all :: bootstrap

# -----------------------------------------------------------------------------
# Installing

# We want to install the stage 2 bootstrapped compiler by default, but we let
# the user override this by saying 'make install stage=1', for example.
ifeq "$(stage)" ""
INSTALL_STAGE = stage=2
else
INSTALL_STAGE =
endif

# Same as default rule, but we pass $(INSTALL_STAGE) to $(MAKE) too
install :: check-packages
	@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \
	for i in $(SUBDIRS); do \
	  echo "------------------------------------------------------------------------"; \
	  echo "== $(MAKE) $@ $(MFLAGS);"; \
	  echo " in $(shell pwd)/$$i"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) --no-print-directory -C $$i $(INSTALL_STAGE) $(MFLAGS) $@; \
	  if [ $$? -eq 0 -o $$x_on_err -eq 0 ] ;  then true; else exit 1; fi; \
	done

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
# These files need to be in the InstallShield
# INSTALL_DATAS rather than INSTALL_DOCS is used so these files go
# in the top-level directory of the distribution
INSTALL_DATAS += ANNOUNCE LICENSE README
endif

# If installing on Windows with MinGW32, copy the gcc compiler, headers and libs
# and the perl interpreter and dll into the GHC prefix directory.
# Gcc and Perl source locations derived from configuration data.
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
ifneq "$(WhatGccIsCalled)" ""
install ::
	-mkdir $(prefix)/gcc-lib
	-mkdir $(prefix)/include
	-mkdir $(prefix)/include/mingw
	-cp -rp $(GccDir)../include/* $(prefix)/include/mingw
	-cp -rp $(GccDir)../lib/gcc-lib/mingw32/$(GccVersion)/* $(prefix)/gcc-lib
	-cp -rp $(GccDir)../lib/gcc/mingw32/$(GccVersion)/* $(prefix)/gcc-lib
	-cp -rp $(GccDir)../libexec/gcc/mingw32/$(GccVersion)/* $(prefix)/gcc-lib
	-cp $(GccDir)../lib/*.* $(prefix)/gcc-lib
	-cp $(GccDir)gcc.exe $(prefix)
	-cp $(GccDir)as.exe $(prefix)/gcc-lib
	-cp $(GccDir)ld.exe $(prefix)/gcc-lib
	-cp $(GccDir)dllwrap.exe $(prefix)/gcc-lib
	-cp $(GccDir)dlltool.exe $(prefix)/gcc-lib
	-cp $(GhcDir)../perl.exe $(prefix)
	-cp $(GhcDir)../perl56.dll $(prefix)
endif
endif

install-docs ::
	@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \
	for i in $(SUBDIRS); do \
	  echo "------------------------------------------------------------------------"; \
	  echo "== $(MAKE) $@ $(MFLAGS);"; \
	  echo " in $(shell pwd)/$$i"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) --no-print-directory -C $$i $(INSTALL_STAGE) $(MFLAGS) $@; \
	  if [ $$? -eq 0 -o $$x_on_err -eq 0 ] ;  then true; else exit 1; fi; \
	done

# -----------------------------------------------------------------------------
# Making a binary distribution
#
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
#	binary-dist is a GHC addition for binary distributions
# 

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
BinDistShScripts = ghc-$(ProjectVersion) ghci-$(ProjectVersion) ghc-pkg-$(ProjectVersion) hsc2hs
else
BinDistShScripts =
endif

BinDistPrlScripts = ghcprof
BinDistLibPrlScripts = ghc-asm ghc-split
BinDistBins = hp2ps runghc
BinDistOptBins = runhaskell
BinDistLinks = ghc ghci ghc-pkg
BinDistLibSplicedFiles = package.conf
BinDistDirs = includes compiler docs driver libraries rts utils

BIN_DIST_NAME=ghc-$(ProjectVersion)
BIN_DIST_TMPDIR=$(FPTOOLS_TOP_ABS)

BIN_DIST_TARBALL=ghc-$(ProjectVersion)-$(TARGETPLATFORM).tar.bz2

BIN_DIST_TOP= distrib/Makefile-bin.in \
	      distrib/configure-bin.ac \
	      distrib/INSTALL \
	      distrib/README \
	      ANNOUNCE \
	      LICENSE \
	      utils/mkdirhier/mkdirhier \
	      install-sh \
	      config.guess \
	      config.sub   \
	      aclocal.m4

ifeq "$(darwin_TARGET_OS)" "1"
BIN_DIST_TOP+=mk/fix_install_names.sh
endif

.PHONY: binary-dist-pre binary-dist binary-pack

binary-dist:: binary-dist-pre

binary-dist-pre::
ifeq "$(BIN_DIST)" ""
	@echo "WARNING: To run the binary-dist target, you need to set BIN_DIST=1 in mk/build.mk" && exit 1
endif
	-rm -rf $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)
	-$(RM) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME).tar.gz
	@for i in $(BinDistDirs); do 		 	 \
	  if test -d "$$i"; then 			 \
	   echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM); \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM); \
	   echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM); \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM); \
	   echo $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share; \
	   $(MKDIRHIER) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share; \
	   echo $(MAKE) -C $$i $(MFLAGS) $(INSTALL_STAGE) install \
		prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		exec_prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		bindir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM) \
		libdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM) \
		libexecdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM) \
		datadir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share; \
	   $(MAKE) -C $$i $(MFLAGS) $(INSTALL_STAGE) install \
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
	echo "package = ghc" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "version = $(ProjectVersion)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_SH_SCRIPTS = $(BinDistShScripts)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_PRL_SCRIPTS = $(BinDistPrlScripts)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_LIB_PRL_SCRIPTS = $(BinDistLibPrlScripts)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_LIB_SPLICED_FILES = $(BinDistLibSplicedFiles)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_BINS = $(BinDistBins)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_OPT_BINS = $(BinDistOptBins)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	echo "PACKAGE_LINKS = $(BinDistLinks)" >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	cat $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile-bin.in >> $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/Makefile.in
	@echo "Generating a shippable configure script.."
	$(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/configure-bin.ac $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/configure.ac
	( cd $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME); autoconf )
#
# binary dist'ing the documentation.  
# The default documentation to build/install is given below; overrideable
# via build.mk or the 'make' command-line.

ifndef BINDIST_DOC_WAYS

ifneq "$(XSLTPROC)" ""
BINDIST_DOC_WAYS = html
ifneq "$(FOP)" ""
BINDIST_DOC_WAYS += ps pdf
else
ifneq "$(PDFXMLTEX)" ""
BINDIST_DOC_WAYS += pdf
endif
ifneq "$(XMLTEX)" ""
ifneq "$(DVIPS)" ""
BINDIST_DOC_WAYS += ps
endif # DVIPS
endif # XMLTEX
endif # FOP
endif # XSLTPROC

endif # BINDIST_DOC_WAYS

binary-dist ::
ifneq "$(DIR_DOCBOOK_XSL)" ""
	@for i in $(BinDistDirs); do 		 	 	\
	  if test -d "$$i"; then 			 	\
	    $(MAKE) -C $$i $(MFLAGS) $(BINDIST_DOC_WAYS); 	\
	    echo $(MAKE) -C $$i $(MFLAGS) install-docs XMLDocWays="$(BINDIST_DOC_WAYS)" \
		prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) 	\
		exec_prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		bindir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM) \
		libdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM) \
		libexecdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM) \
		datadir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share; \
	    $(MAKE) -C $$i $(MFLAGS) install-docs XMLDocWays="$(BINDIST_DOC_WAYS)" \
		prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) 	\
		exec_prefix=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME) \
		bindir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM) \
		libdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM) \
		libexecdir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM) \
		datadir=$(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/share; \
	  fi \
	done
endif

# Rename scripts to $i.prl and $i.sh where necessary.
# ToDo: do this in a cleaner way...

ifneq "$(BinDistPrlScripts)" ""
binary-dist::
	@for i in $(BinDistPrlScripts); do \
	     echo "Renaming $$i to $$i.prl"; \
	    $(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$$i.prl; \
	done
endif

ifneq "$(BinDistLibPrlScripts)" ""
binary-dist::
	@for i in $(BinDistLibPrlScripts); do \
	     echo "Renaming $$i to $$i.prl"; \
	    $(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/lib/$(TARGETPLATFORM)/$$i.prl; \
	done
endif

ifneq "$(BinDistShScripts)" ""
binary-dist::
	@for i in $(BinDistShScripts); do \
	    if test -x $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$$i ; then \
	    	echo "Renaming $$i to $$i.sh"; \
	    	$(MV) $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$$i  $(BIN_DIST_TMPDIR)/$(BIN_DIST_NAME)/bin/$(TARGETPLATFORM)/$$i.sh; \
	    fi \
	done
endif

# Tar up the distribution and build a manifest
binary-dist ::
	( cd $(BIN_DIST_TMPDIR); tar cf - $(BIN_DIST_NAME) | bzip2 >$(BIN_DIST_TARBALL) )
	( cd $(BIN_DIST_TMPDIR); bunzip2 -c $(BIN_DIST_TARBALL) | tar tf - | sed "s/^ghc-$(ProjectVersion)/fptools/" | sort >bin-manifest-$(ProjectVersion) )

# Upload the distribution
ifneq "$(PublishLocation)" ""
binary-dist ::
	@for i in 0 1 2 3 4 5 6 7 8 9; do \
		echo "Try $$i: $(PublishCp) $(SRC_DIST_EXTRALIBS_TARBALL) $(PublishLocation)"; \
		if $(PublishCp) $(SRC_DIST_EXTRALIBS_TARBALL) $(PublishLocation); then break; fi\
	done
endif

binary-dist::
	@echo "Mechanical and super-natty! Inspect the result and *if* happy; freeze, sell and get some sleep!"

# -----------------------------------------------------------------------------
# Building source distributions
#
# Do it like this: 
#
#	$ make
#	$ make dist
#
# WARNING: `make dist' calls `make distclean' before tarring up the tree.
#

.PHONY: dist

#
# Directory in which we're going to build the src dist
#
SRC_DIST_NAME=ghc-$(ProjectVersion)
SRC_DIST_DIR=$(shell pwd)/$(SRC_DIST_NAME)

#
# Files to include in source distributions
#
SRC_DIST_DIRS += mk docs distrib $(filter-out docs distrib,$(SUBDIRS))
SRC_DIST_FILES += \
	configure.ac config.guess config.sub configure \
	aclocal.m4 README ANNOUNCE HACKING LICENSE Makefile install-sh \
	ghc.spec.in VERSION

# -----------------------------------------------------------------------------
# Source distributions

# A source dist is built from a complete build tree, because we
# require some extra files not contained in a darcs checkout: the
# output from Happy and Alex, for example.
# 
# The steps performed by 'make dist' are as follows:
#   - create a complete link-tree of the current build tree in /tmp
#   - run 'make distclean' on that tree
#   - remove a bunch of other files that we know shouldn't be in the dist
#   - tar up first the extralibs package, then the main source package

EXTRA_LIBS=$(patsubst %, $(SRC_DIST_NAME)/libraries/%, $(shell cat libraries/extra-packages))

SRC_DIST_TARBALL = ghc-$(ProjectVersion)-src.tar.bz2
SRC_DIST_EXTRALIBS_TARBALL = ghc-$(ProjectVersion)-src-extralibs.tar.bz2

dist ::
	$(RM) -rf $(SRC_DIST_DIR)
	$(RM) $(SRC_DIST_NAME).tar.gz
	mkdir $(SRC_DIST_DIR)
	( cd $(SRC_DIST_DIR) \
	  && for i in $(SRC_DIST_DIRS); do mkdir $$i; (cd $$i && lndir $(FPTOOLS_TOP_ABS)/$$i ); done \
	  && for i in $(SRC_DIST_FILES); do $(LN_S) $(FPTOOLS_TOP_ABS)/$$i .; done \
	  && $(MAKE) distclean \
	  && $(RM) -rf compiler/stage[123] mk/build.mk \
	  && $(FIND) $(SRC_DIST_DIRS) \( -name _darcs -o -name SRC -o -name "autom4te*" -o -name "*~" -o -name ".cvsignore" -o -name "\#*" -o -name ".\#*" -o -name "log" -o -name "*-SAVE" -o -name "*.orig" -o -name "*.rej" \) -print | xargs $(RM) -rf \
	)
	tar chf - $(EXTRA_LIBS) | bzip2 >$(FPTOOLS_TOP_ABS)/$(SRC_DIST_EXTRALIBS_TARBALL)
	$(RM) -rf $(EXTRA_LIBS)
	tar chf - $(SRC_DIST_NAME) 2>$src_log | bzip2 >$(FPTOOLS_TOP_ABS)/$(SRC_DIST_TARBALL)

# Upload the distribution(s)
# Retrying is to work around buggy firewalls that corrupt large file transfers
# over SSH.
ifneq "$(PublishLocation)" ""
dist ::
	@for i in 0 1 2 3 4 5 6 7 8 9; do \
		echo "Try $$i: $(PublishCp) $(SRC_DIST_EXTRALIBS_TARBALL) $(PublishLocation)"; \
		if $(PublishCp) $(SRC_DIST_EXTRALIBS_TARBALL) $(PublishLocation); then break; fi\
	done
	@for i in 0 1 2 3 4 5 6 7 8 9; do \
		echo "Try $$i: $(PublishCp) $(SRC_DIST_TARBALL) $(PublishLocation)"; \
		if $(PublishCp) $(SRC_DIST_TARBALL) $(PublishLocation); then break; fi\
	done
endif

# -----------------------------------------------------------------------------
# HC file bundles

hc-file-bundle :
	$(RM) -r ghc-$(ProjectVersion)
	$(LN_S) . ghc-$(ProjectVersion)
	$(FIND) ghc-$(ProjectVersion)/compiler \
	     ghc-$(ProjectVersion)/utils \
	     ghc-$(ProjectVersion)/compat \
	     ghc-$(ProjectVersion)/libraries -follow \
	  \( -name "*.hc" -o -name "*_hsc.[ch]" -o -name "*_stub.[ch]" \) -print > hc-files-to-go
	for f in `$(FIND) ghc-$(ProjectVersion)/compiler ghc-$(ProjectVersion)/utils ghc-$(ProjectVersion)/libraries -name "*.hsc" -follow -print` ""; do \
	     if test "x$$f" != "x" && test -e `echo "$$f" | sed 's/hsc$$/hs/g'`; then \
	        echo `echo "$$f" | sed 's/hsc$$/hs/g' ` >> hc-files-to-go ; \
	     fi; \
	done;
	for f in `$(FIND) ghc-$(ProjectVersion)/compiler ghc-$(ProjectVersion)/rts -name "*.cmm" -follow -print` ""; do \
	     if test "x$$f" != "x"; then \
	        echo `echo "$$f" | sed 's/cmm$$/hc/g' ` >> hc-files-to-go ; \
	     fi; \
	done;
	echo ghc-$(ProjectVersion)/libraries/base/GHC/PrimopWrappers.hs >> hc-files-to-go
	echo ghc-$(ProjectVersion)/compiler/parser/Parser.hs >> hc-files-to-go
	echo ghc-$(ProjectVersion)/compiler/parser/ParserCore.hs >> hc-files-to-go
	echo ghc-$(ProjectVersion)/compiler/main/ParsePkgConf.hs >> hc-files-to-go
	echo ghc-$(ProjectVersion)/libraries/haskell-src/Language/Haskell/Parser.hs >> hc-files-to-go
	tar czf ghc-$(ProjectVersion)-$(TARGETPLATFORM)-hc.tar.gz `cat hc-files-to-go`

# -----------------------------------------------------------------------------
# Cleaning

CLEAN_FILES += hc-files-to-go *-hc.tar.gz

DIST_CLEAN_FILES += config.cache config.status mk/config.h mk/stamp-h \
	ghc.spec docs/users_guide/ug-book.xml

# don't clean config.mk: it's needed when cleaning stuff later on
LATE_DIST_CLEAN_FILES += mk/config.mk 

# VERSION is shipped in a source dist
MAINTAINER_CLEAN_FILES += VERSION

extraclean::
	$(RM) -rf autom4te.cache

# -----------------------------------------------------------------------------

# Turn off target.mk's rules for 'all', 'boot' and 'install'.
NO_BOOT_TARGET=YES
NO_ALL_TARGET=YES
NO_INSTALL_TARGET=YES

include $(TOP)/mk/target.mk

# -----------------------------------------------------------------------------

