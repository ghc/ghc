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
SUBDIRS_BUILD = gmp includes rts compat compiler docs utils driver
else
SUBDIRS_BUILD = gmp includes compat utils driver docs compiler rts
endif

SUBDIRS = gmp includes compat utils driver docs rts libraries compiler

# Sanity check that all the boot libraries are in the tree, to catch
# failure to run darcs-all.
check-packages :
	@ds=`cat libraries/boot-packages`;\
	for d in $$ds; do \
	  if test ! -d libraries/$$d; then \
	     echo "Looks like you're missing libraries/$$d,"; \
	     echo "maybe you haven't done './darcs-all get'?"; \
	     exit 1; \
	  fi \
	done
	@if test ! -f libraries/base/configure; then \
	    echo "Looks like you're missing base's configure script."; \
	    echo "Did you run 'sh boot' at the top level?"; \
	    exit 1; \
	fi

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
ifneq "$(WhatGccIsCalled)" ""
GCC_LIB_DEP = stamp.inplace-gcc-lib
endif
endif

stage1 : $(GCC_LIB_DEP) check-packages
	$(MAKE) -C utils/mkdependC boot
	@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \
	for i in $(SUBDIRS_BUILD); do \
	  echo "------------------------------------------------------------------------"; \
	  echo "== $(MAKE) boot $(MFLAGS);"; \
	  echo " in $(shell pwd)/$$i"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) --no-print-directory -C $$i $(MFLAGS) boot; \
	  if [ $$? -eq 0 -o $$x_on_err -eq 0 ] ;  then true; else exit 1; fi; \
	done; \
	for i in $(SUBDIRS_BUILD); do \
	  echo "------------------------------------------------------------------------"; \
	  echo "== $(MAKE) all $(MFLAGS);"; \
	  echo " in $(shell pwd)/$$i"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) --no-print-directory -C $$i $(MFLAGS) all; \
	  if [ $$? -eq 0 -o $$x_on_err -eq 0 ] ;  then true; else exit 1; fi; \
	done
	$(MAKE) -C libraries boot
	$(MAKE) -C libraries all

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

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
ifneq "$(WhatGccIsCalled)" ""
all :: stamp.inplace-gcc-lib

.PHONY: stamp.inplace-gcc-lib

# This is a hack to make Cabal able to find ld when we run tests with
# the inplace ghc. We should probably install all the gcc stuff in our
# tree somewhere, and then have install copy it from there rather than
# from the filesystem.
stamp.inplace-gcc-lib:
	$(RM) -r compiler/gcc-lib
	mkdir compiler/gcc-lib
	cp $(LD) compiler/gcc-lib
	touch $@

clean ::
	$(RM) -r compiler/gcc-lib
	$(RM) -f inplace-gcc-lib
endif
endif

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

# Install gcc-extra-opts
install ::
	$(INSTALL_DIR) $(DESTDIR)$(libdir)
	$(INSTALL_DATA) $(INSTALL_OPTS) extra-gcc-opts $(DESTDIR)$(libdir)

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

binary-dist::
	-rm -rf $(BIN_DIST_DIR)
	-$(RM) $(BIN_DIST_DIR).tar.gz

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"

binary-dist::
	$(MAKE) prefix=$(BIN_DIST_DIR) install

binary-dist::
	cd $(BIN_DIST_DIR) && ../distrib/prep-bin-dist-mingw

binary-dist::
	$(MKDIRHIER) $(BIN_DIST_DIR)/icons
	cp distrib/hsicon.ico $(BIN_DIST_DIR)/icons

else

BinDistDirs = includes compiler docs rts

BIN_DIST_TOP= distrib/Makefile \
              distrib/configure-bin.ac \
              distrib/INSTALL \
              distrib/README \
              ANNOUNCE \
              LICENSE \
              install-sh \
	      extra-gcc-opts.in \
              config.guess \
              config.sub   \
              aclocal.m4

ifeq "$(darwin_TARGET_OS)" "1"
BIN_DIST_TOP+=mk/fix_install_names.sh
endif

.PHONY: binary-dist-pre% binary-dist binary-pack

binary-dist:: binary-dist-pre

binary-dist-pre::
	$(MKDIRHIER) $(BIN_DIST_DIR)/mk
	echo 'include $$(TOP)/Makefile-vars' >  $(BIN_DIST_DIR)/mk/boilerplate.mk
	echo 'include $$(TOP)/mk/package.mk' >  $(BIN_DIST_DIR)/mk/target.mk
	echo 'include $$(TOP)/mk/install.mk' >> $(BIN_DIST_DIR)/mk/target.mk
	echo 'include $$(TOP)/mk/recurse.mk' >> $(BIN_DIST_DIR)/mk/target.mk
	echo ''                              >  $(BIN_DIST_DIR)/mk/compat.mk
	cp mk/package.mk $(BIN_DIST_DIR)/mk/
	cp mk/install.mk $(BIN_DIST_DIR)/mk/
	cp mk/recurse.mk $(BIN_DIST_DIR)/mk/
	$(MKDIRHIER) $(BIN_DIST_DIR)/lib/$(TARGETPLATFORM)
	$(MKDIRHIER) $(BIN_DIST_DIR)/share

binary-dist::
	$(MAKE) -C gmp      binary-dist DOING_BIN_DIST=YES
	$(MAKE) -C includes binary-dist DOING_BIN_DIST=YES
	$(MAKE) -C compiler binary-dist DOING_BIN_DIST=YES $(INSTALL_STAGE)
	# XXX $(MAKE) -C docs     binary-dist DOING_BIN_DIST=YES
	$(MAKE) -C rts      binary-dist DOING_BIN_DIST=YES
	$(MAKE) -C driver   binary-dist DOING_BIN_DIST=YES
	$(MAKE) -C utils    binary-dist DOING_BIN_DIST=YES

VARFILE=$(BIN_DIST_DIR)/Makefile-vars.in

binary-dist::
	@for i in $(BIN_DIST_TOP); do \
	  if test -f "$$i"; then \
	     echo cp $$i $(BIN_DIST_DIR); \
	     cp $$i $(BIN_DIST_DIR); \
	  fi; \
	done;
	@echo "Configuring the Makefile for this project..."
	echo                                                         >  $(VARFILE)
	echo "package = ghc"                                         >> $(VARFILE)
	echo "version = $(ProjectVersion)"                           >> $(VARFILE)
	echo "ProjectVersion = $(ProjectVersion)"                    >> $(VARFILE)
	echo "HaveLibGmp = $(HaveLibGmp)"                            >> $(VARFILE)
	echo "GhcLibsWithUnix = $(GhcLibsWithUnix)"                  >> $(VARFILE)
	echo "GhcWithInterpreter = $(GhcWithInterpreter)"            >> $(VARFILE)
	echo "GhcHasReadline = $(GhcHasReadline)"                    >> $(VARFILE)
	echo "BootingFromHc = $(BootingFromHc)"                      >> $(VARFILE)
	cat distrib/Makefile-bin-vars.in                             >> $(VARFILE)
	@echo "Generating a shippable configure script.."
	$(MV) $(BIN_DIST_DIR)/configure-bin.ac $(BIN_DIST_DIR)/configure.ac
	( cd $(BIN_DIST_DIR); autoreconf )

#
# binary dist'ing the documentation.  
# The default documentation to build/install is given below; overrideable
# via build.mk or the 'make' command-line.
#
# If BINDIST_DOC_WAYS is set, use that
# If XMLDocWays is set, use that
# Otherwise, figure out what we can build based on configure results

ifndef BINDIST_DOC_WAYS

ifneq "$(XMLDocWays)" ""
BINDIST_DOC_WAYS = $(XMLDocWays)
else
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
endif # XMLDocWays

endif # BINDIST_DOC_WAYS

ifneq "$(DIR_DOCBOOK_XSL)" ""
.PHONY: binary-dist-doc-%

BINARY_DIST_DOC_RULES=$(foreach d,$(BinDistDirs),binary-dist-doc-$d)

binary-dist :: $(BINARY_DIST_DOC_RULES)

$(BINARY_DIST_DOC_RULES): binary-dist-doc-%:
	$(MAKE) -C $* $(MFLAGS) $(BINDIST_DOC_WAYS)
	$(MAKE) -C $* $(MFLAGS) install-docs \
			MAKING_BIN_DIST=1 \
	        XMLDocWays="$(BINDIST_DOC_WAYS)" \
	        prefix=$(BIN_DIST_DIR) \
	        exec_prefix=$(BIN_DIST_DIR) \
	        bindir=$(BIN_DIST_DIR)/bin/$(TARGETPLATFORM) \
	        libdir=$(BIN_DIST_DIR)/lib/$(TARGETPLATFORM) \
	        libexecdir=$(BIN_DIST_DIR)/lib/$(TARGETPLATFORM) \
	        datadir=$(BIN_DIST_DIR)/share
endif

.PHONY: binary-dist-doc-%

binary-dist::
	$(MAKE) -C libraries binary-dist

endif

# Tar up the distribution and build a manifest
binary-dist :: tar-binary-dist

.PHONY: tar-binary-dist
tar-binary-dist:
	( cd $(BIN_DIST_TOPDIR_ABS); tar cf - $(BIN_DIST_NAME) | bzip2 >$(BIN_DIST_TARBALL) )
	( cd $(BIN_DIST_TOPDIR_ABS); bunzip2 -c $(BIN_DIST_TARBALL) | tar tf - | sed "s/^ghc-$(ProjectVersion)/fptools/" | sort >bin-manifest-$(ProjectVersion) )

PUBLISH_FILES = $(BIN_DIST_TARBALL)

# Upload the distribution and documentation
ifneq "$(ISCC)" ""
WINDOWS_INSTALLER_BASE = ghc-$(ProjectVersion)-$(TARGETPLATFORM)
WINDOWS_INSTALLER = $(WINDOWS_INSTALLER_BASE)$(exeext)

PUBLISH_FILES += $(WINDOWS_INSTALLER)

binary-dist :: generate-windows-installer

.PHONY: generate-windows-installer
generate-windows-installer ::
	$(SED) "s/@VERSION@/$(ProjectVersion)/" distrib/ghc.iss | $(ISCC) /O. /F$(WINDOWS_INSTALLER_BASE) -
endif

# Upload the distribution and documentation
ifneq "$(PublishLocation)" ""
publish :: publish-binary-dist
endif

.PHONY: publish-binary-dist
publish-binary-dist ::
	@for f in $(PUBLISH_FILES); do \
	    for i in 0 1 2 3 4 5 6 7 8 9; do \
	        echo "Try $$i: $(PublishCp) $$f $(PublishLocation)/dist"; \
	        if $(PublishCp) $$f $(PublishLocation)/dist; then break; fi; \
	    done \
	done

# You need to first make binddisttest, and then run
#     make publish 'prefix=$(BIN_DIST_INST_DIR)'
# for this to find the right place.

# We assume that Windows means Cygwin, as we can't just use docdir
# unchanged or rsync (really SSH?) thinks that c:/foo means /foo on
# the machine c.

ifeq "$(Windows)" "YES"
PUBLISH_DOCDIR = $(shell cygpath --unix $(docdir))
else
PUBLISH_DOCDIR = $(docdir)
endif

publish-binary-dist ::
	$(PublishCp) -r $(PUBLISH_DOCDIR)/* $(PublishLocation)/docs

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
SRC_DIST_DIRS += mk docs distrib bindisttest $(filter-out docs distrib,$(SUBDIRS))
SRC_DIST_FILES += \
	configure.ac config.guess config.sub configure \
	aclocal.m4 README ANNOUNCE HACKING LICENSE Makefile install-sh \
	ghc.spec.in extra-gcc-opts.in VERSION boot

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

VERSION :
	echo $(ProjectVersion) >VERSION

dist :: VERSION

dist ::
	$(RM) -rf $(SRC_DIST_DIR)
	$(RM) $(SRC_DIST_NAME).tar.gz
	mkdir $(SRC_DIST_DIR)
	( cd $(SRC_DIST_DIR) \
	  && for i in $(SRC_DIST_DIRS); do mkdir $$i; (cd $$i && lndir $(FPTOOLS_TOP_ABS)/$$i ); done \
	  && for i in $(SRC_DIST_FILES); do $(LN_S) $(FPTOOLS_TOP_ABS)/$$i .; done \
	  && $(MAKE) distclean \
	  && if test -f $(FPTOOLS_TOP_ABS)/libraries/haskell-src/dist/build/Language/Haskell/Parser.hs; then $(CP) $(FPTOOLS_TOP_ABS)/libraries/haskell-src/dist/build/Language/Haskell/Parser.hs libraries/haskell-src/Language/Haskell/ ; fi \
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
		echo "Try $$i: $(PublishCp) $(SRC_DIST_EXTRALIBS_TARBALL) $(PublishLocation)/dist"; \
		if $(PublishCp) $(SRC_DIST_EXTRALIBS_TARBALL) $(PublishLocation)/dist; then break; fi\
	done
	@for i in 0 1 2 3 4 5 6 7 8 9; do \
		echo "Try $$i: $(PublishCp) $(SRC_DIST_TARBALL) $(PublishLocation)/dist"; \
		if $(PublishCp) $(SRC_DIST_TARBALL) $(PublishLocation)/dist; then break; fi\
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
	ghc.spec docs/users_guide/ug-book.xml extra-gcc-opts

# don't clean config.mk: it's needed when cleaning stuff later on
LATE_DIST_CLEAN_FILES += mk/config.mk 

# VERSION is shipped in a source dist
MAINTAINER_CLEAN_FILES += VERSION

extraclean::
	$(RM) -rf autom4te.cache

clean distclean ::
	$(MAKE) -C bindisttest $@
	if test -d testsuite; then $(MAKE) -C testsuite $@; fi

# -----------------------------------------------------------------------------

# Turn off target.mk's rules for 'all', 'boot' and 'install'.
NO_BOOT_TARGET=YES
NO_ALL_TARGET=YES
NO_INSTALL_TARGET=YES

include $(TOP)/mk/target.mk

# -----------------------------------------------------------------------------

