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
#	framework-pkg  [MacOS only]
#		Builds /Library/Frameworks/GHC.framework wrapped into a Mac
#		installer package
#
#	framework-binary-dist  [MacOS only]
#		Builds GHC.framework encapsulating a binary distribution
#		(to give a relocatable framework)
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
SUBDIRS_BUILD = includes compiler docs utils driver
else
SUBDIRS_BUILD = includes utils driver docs compiler libraries/Cabal/doc
endif

SUBDIRS = gmp libffi includes utils docs rts compiler ghc driver libraries libraries/Cabal/doc

check-all: check-packages

# Sanity check that all the boot libraries are in the tree, to catch
# failure to run darcs-all.
check-packages :
	@ds=`grep "^[^# ][^ ]*  *[^ ][^ ]*  *[^ ][^ ]*$$" packages | sed "s/ .*//"`;\
	for d in $$ds; do \
	  if test ! -d $$d; then \
	     echo "Looks like you're missing $$d,"; \
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

stage1 : $(GCC_LIB_DEP) check-all
	$(MAKE) -C libraries boot
	$(MAKE) -C gmp       all
	$(MAKE) -C libffi    all
	$(MAKE) -C utils/mkdependC boot
	$(MAKE) -C utils with-bootstrapping-compiler
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
	$(MAKE) -C rts boot
	$(MAKE) -C rts
	$(MAKE) -C libraries all

# When making distributions (i.e., whether with binary-dist or using the 
# vanilla install target to create an installer package), we can have problems
# if some things (e.g. ghc-pkg) are compiled with the bootstrapping compiler 
# and some (e.g. the stage 2 compiler) with the stage1 compiler. See #1860 for
# an example.  Thus, we explicitly build a second version with the stage 1 
# compiler of all utils that get installed and of all extra support binaries
# includes in binary dists.
stage2 : check-all
	$(MAKE) -C compiler stage=2 boot
	$(MAKE) -C compiler stage=2
	$(MAKE) -C utils with-stage-2
ifeq "$(HADDOCK_DOCS)" "YES"
	$(MAKE) -C libraries doc
	$(MAKE) -C compiler  doc stage=2
endif

stage3 : check-all
	$(MAKE) -C compiler stage=3 boot
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
	$(RM) -r ghc/gcc-lib
	mkdir ghc/gcc-lib
	cp $(LD) ghc/gcc-lib
	touch $@

clean ::
	$(RM) -r ghc/gcc-lib
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

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

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
	-$(RM) $(BIN_DIST_TAR_BZ2)

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"

binary-dist::
	$(MAKE) prefix=$(BIN_DIST_DIR) install
	$(MAKE) prefix=$(BIN_DIST_DIR) install-docs

binary-dist::
	cd $(BIN_DIST_DIR) && $(SHELL) ../distrib/prep-bin-dist-mingw

binary-dist::
	$(MKDIRHIER) $(BIN_DIST_DIR)/icons
	cp distrib/hsicon.ico $(BIN_DIST_DIR)/icons

# Tar up the distribution and build a manifest
binary-dist :: tar-binary-dist

.PHONY: tar-binary-dist
tar-binary-dist:
	( cd $(BIN_DIST_TOPDIR_ABS); $(TAR) cf - $(BIN_DIST_NAME) | bzip2 > $(BIN_DIST_TAR_BZ2) )
	( cd $(BIN_DIST_TOPDIR_ABS); bunzip2 -c $(BIN_DIST_TAR_BZ2) | $(TAR) tf - | sed "s/^ghc-$(ProjectVersion)/fptools/" | sort >$(FPTOOLS_TOP_ABS)/bin-manifest-$(ProjectVersion) )

else

.PHONY: binary-dist

BIN_DIST_VARFILE=$(BIN_DIST_PREP)/Makefile-vars.in

WHERE_AM_I = $(BIN_DIST_NAME)

binary-dist::
# For the most part we will be putting filenames in $(BIN_DIST_LIST),
# and telling tar to tar all of those files up. So to start with we
# remove $(BIN_DIST_LIST) so that we can start with an empty slate
	$(RM) -f $(BIN_DIST_LIST)
# Now we add a few files from mk/ to $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/mk/package.mk     >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/mk/install.mk     >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/mk/recurse.mk     >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/mk/cabal.mk       >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/mk/cabal-flags.mk >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/mk/fptools.css    >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/ANNOUNCE          >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/LICENSE           >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/install-sh        >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/extra-gcc-opts.in >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/config.guess      >> $(BIN_DIST_LIST)
	echo $(WHERE_AM_I)/config.sub        >> $(BIN_DIST_LIST)
# Now we recurse into various subdirectories and tell them to add and
# files that they want into $(BIN_DIST_LIST)
# XXX Ug, this duplication of "-C foo WHERE_AM_I=.../foo" isn't nice.
	$(MAKE) -C gmp                 binary-dist WHERE_AM_I=$(WHERE_AM_I)/gmp
	$(MAKE) -C includes            binary-dist WHERE_AM_I=$(WHERE_AM_I)/includes
	$(MAKE) -C compiler            binary-dist WHERE_AM_I=$(WHERE_AM_I)/compiler $(INSTALL_STAGE)
	$(MAKE) -C ghc                 binary-dist WHERE_AM_I=$(WHERE_AM_I)/ghc      $(INSTALL_STAGE)
	$(MAKE) -C rts                 binary-dist WHERE_AM_I=$(WHERE_AM_I)/rts
	$(MAKE) -C driver              binary-dist WHERE_AM_I=$(WHERE_AM_I)/driver
	$(MAKE) -C utils               binary-dist WHERE_AM_I=$(WHERE_AM_I)/utils
	$(MAKE) -C docs                binary-dist WHERE_AM_I=$(WHERE_AM_I)/docs
	$(MAKE) -C libffi              binary-dist WHERE_AM_I=$(WHERE_AM_I)/libffi
	$(MAKE) -C libraries           binary-dist WHERE_AM_I=$(WHERE_AM_I)/libraries
	$(MAKE) -C libraries/Cabal/doc binary-dist WHERE_AM_I=$(WHERE_AM_I)/libraries/Cabal/doc
# Now thinks get messier. Some files we need to move around, rename or
# generate. We do this under $(BIN_DIST_PREP).
	$(RM) -rf    $(BIN_DIST_PREP_DIR)
	$(MKDIRHIER) $(BIN_DIST_PREP)/mk
	echo 'include $$(TOP)/Makefile-vars' >  $(BIN_DIST_PREP)/mk/boilerplate.mk
	echo 'include $$(TOP)/mk/package.mk' >  $(BIN_DIST_PREP)/mk/target.mk
	echo 'include $$(TOP)/mk/install.mk' >> $(BIN_DIST_PREP)/mk/target.mk
	echo 'include $$(TOP)/mk/recurse.mk' >> $(BIN_DIST_PREP)/mk/target.mk
	touch                                   $(BIN_DIST_PREP)/mk/bindist.mk
ifeq "$(darwin_TARGET_OS)" "1"
	cp mk/fix_install_names.sh $(BIN_DIST_PREP)/mk
endif

	cp distrib/Makefile         $(BIN_DIST_PREP)
	cp distrib/INSTALL          $(BIN_DIST_PREP)
	cp distrib/README           $(BIN_DIST_PREP)
	cp distrib/configure-bin.ac $(BIN_DIST_PREP)/configure.ac
# We can't just list aclocal.m4 in $(BIN_DIST_LIST), as it needs to be
# next to configure.ac when we run autoreconf
	cp aclocal.m4               $(BIN_DIST_PREP)
	cd $(BIN_DIST_PREP) && autoreconf
# We need to copy the pwd program that was built with stage1 to where
# the build system expects to find it, i.e. the location the pwd built
# with the bootstrapping compiler normally occupies
	$(MKDIRHIER) $(BIN_DIST_PREP)/utils/pwd
	cp utils/pwd/dist-install/build/pwd/pwd $(BIN_DIST_PREP)/utils/pwd
# And likewise the installPackage program
	$(MKDIRHIER) $(BIN_DIST_PREP)/utils/installPackage/install-inplace/bin
	cp utils/installPackage/dist-install/build/installPackage/installPackage \
	   $(BIN_DIST_PREP)/utils/installPackage/install-inplace/bin

	echo "package = ghc"                              >> $(BIN_DIST_VARFILE)
	echo "version = $(ProjectVersion)"                >> $(BIN_DIST_VARFILE)
	echo "ProjectVersion = $(ProjectVersion)"         >> $(BIN_DIST_VARFILE)
	echo "HaveLibGmp = $(HaveLibGmp)"                 >> $(BIN_DIST_VARFILE)
	echo "GhcLibsWithUnix = $(GhcLibsWithUnix)"       >> $(BIN_DIST_VARFILE)
	echo "GhcWithInterpreter = $(GhcWithInterpreter)" >> $(BIN_DIST_VARFILE)
	echo "GhcHasEditline = $(GhcHasEditline)"         >> $(BIN_DIST_VARFILE)
	echo "BootingFromHc = $(BootingFromHc)"           >> $(BIN_DIST_VARFILE)
	echo "XMLDocWays = $(XMLDocWays)"                 >> $(BIN_DIST_VARFILE)
# We won't actually use xsltproc, but we need to know if it's "" or not
	echo "XSLTPROC = $(XSLTPROC)"                     >> $(BIN_DIST_VARFILE)
	echo "TARGETPLATFORM = $(TARGETPLATFORM)"         >> $(BIN_DIST_VARFILE)
	echo "HADDOCK_DOCS = $(HADDOCK_DOCS)"             >> $(BIN_DIST_VARFILE)
	echo "LATEX_DOCS = $(LATEX_DOCS)"                 >> $(BIN_DIST_VARFILE)
	echo "INTEGER_LIBRARY = $(INTEGER_LIBRARY)"       >> $(BIN_DIST_VARFILE)
	cat distrib/Makefile-bin-vars.in                  >> $(BIN_DIST_VARFILE)

# With that done, we can now build the actual tarball

	$(RM) -f $(BIN_DIST_NAME)
	ln -s . $(BIN_DIST_NAME)
# h means "follow symlinks", e.g. if aclocal.m4 is a symlink to a source
# tree then we want to include the real file, not a symlink to it
	$(TAR) hcf $(BIN_DIST_TAR) -T $(BIN_DIST_LIST)
	cd $(BIN_DIST_PREP_DIR) && $(TAR) rf $(BIN_DIST_TAR) $(BIN_DIST_NAME)
	bzip2 < $(BIN_DIST_TAR) > $(BIN_DIST_TAR_BZ2)
	$(TAR) tf $(BIN_DIST_TAR) | sort > bin-manifest-$(ProjectVersion)
endif

PUBLISH_FILES = $(BIN_DIST_TAR_BZ2)

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
publish ::
	make publish-binary-dist 'prefix=$(BIN_DIST_INST_DIR)'
endif

nTimes = set -e; for i in `seq 1 $(1)`; do echo Try "$$i: $(2)"; if $(2); then break; fi; done

.PHONY: publish-binary-dist
publish-binary-dist ::
	@for f in $(PUBLISH_FILES); \
	    do $(call nTimes,10,$(PublishCp) $$f $(PublishLocation)/dist); \
	done

# You need to "make binddisttest" before publishing the docs, as it
# works by publish setting $(prefix) to inside the binddisttest
# directory so $(docdir) points to the right place.

# We assume that Windows means Cygwin, as we can't just use docdir
# unchanged or rsync (really SSH?) thinks that c:/foo means /foo on
# the machine c.

ifeq "$(Windows)" "YES"
PUBLISH_DOCDIR = $(shell cygpath --unix $(docdir))
else
PUBLISH_DOCDIR = $(docdir)
endif

publish-binary-dist ::
	$(call nTimes,10,$(PublishCp) -r "$(PUBLISH_DOCDIR)"/* $(PublishLocation)/docs)

binary-dist::
	@echo "Mechanical and super-natty! Inspect the result and *if* happy; freeze, sell and get some sleep!"

# -----------------------------------------------------------------------------
# Building MacOS installer packages
#
# 'framework-pkg'
#	Create an installer package for /Library/Frameworks/GHC.framework
#
# 'framework-binary-dist'
#	Create an installer package for GHC.framework encapsulating a
#	binary-dist to make it relocatable
#	FIXME: This is only partially implemented so far

ifeq "$(darwin_TARGET_OS)" "1"

.PHONY: framework-pkg framework-binary-dist

framework-pkg:
	$(MAKE) -C distrib/MacOS framework-pkg

framework-binary-dist:
	$(MAKE) -C distrib/MacOS framework-binary-dist

endif

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
SRC_DIST_DIRS += mk docs distrib bindisttest $(filter-out docs distrib libraries/Cabal/doc,$(SUBDIRS))
SRC_DIST_FILES += \
	configure.ac config.guess config.sub configure \
	aclocal.m4 README ANNOUNCE HACKING LICENSE Makefile install-sh \
	ghc.spec.in ghc.spec extra-gcc-opts.in VERSION boot

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

EXTRA_LIBS=$(patsubst %, $(SRC_DIST_NAME)/%, $(shell grep -E "extralibs|dph" packages | grep -v "^\#" | sed "s/ .*//"))

SRC_DIST_TARBALL = ghc-$(ProjectVersion)-src.tar.bz2
SRC_DIST_EXTRALIBS_TARBALL = ghc-$(ProjectVersion)-src-extralibs.tar.bz2

VERSION :
	echo $(ProjectVersion) >VERSION

dist :: VERSION

# Use:
#     $(call copy_generated_compiler_file,cmm,CmmLex,x)
# to copy the generated file that replaces compiler/cmm/CmmLex.x
# XXX Should this be unconditional? Do we want to support making a src dist
# from an unbuilt tree?
copy_generated_compiler_file = \
  if test -f $(FPTOOLS_TOP_ABS)/compiler/dist-stage2/build/$2.hs; \
  then \
    $(CP) $(FPTOOLS_TOP_ABS)/compiler/dist-stage2/build/$2.hs compiler/$1/ ; \
    mv compiler/$1/$2.$3 compiler/$1/$2.$3.source ; \
  fi
copy_generated_util_file = \
  if test -f $(FPTOOLS_TOP_ABS)/utils/$1/dist-install/build/$1/$1-tmp/$2.hs; \
  then \
    $(CP) $(FPTOOLS_TOP_ABS)/utils/$1/dist-install/build/$1/$1-tmp/$2.hs utils/$1/ ; \
    mv utils/$1/$2.$3 utils/$1/$2.$3.source ; \
  fi

dist ::
	$(RM) -rf $(SRC_DIST_DIR)
	$(RM) $(SRC_DIST_NAME).tar.gz
	mkdir $(SRC_DIST_DIR)
	( cd $(SRC_DIST_DIR) \
	  && for i in $(SRC_DIST_DIRS); do mkdir $$i; (cd $$i && lndir $(FPTOOLS_TOP_ABS)/$$i ); done \
	  && for i in $(SRC_DIST_FILES); do $(LN_S) $(FPTOOLS_TOP_ABS)/$$i .; done \
	  && $(MAKE) distclean \
	  && if test -f $(FPTOOLS_TOP_ABS)/libraries/haskell-src/dist/build/Language/Haskell/Parser.hs; then $(CP) $(FPTOOLS_TOP_ABS)/libraries/haskell-src/dist/build/Language/Haskell/Parser.hs libraries/haskell-src/Language/Haskell/ ; mv libraries/haskell-src/Language/Haskell/Parser.ly libraries/haskell-src/Language/Haskell/Parser.ly.source ; fi \
	  && $(call copy_generated_compiler_file,cmm,CmmLex,x) \
	  && $(call copy_generated_compiler_file,cmm,CmmParse,y) \
	  && $(call copy_generated_compiler_file,main,ParsePkgConf,y) \
	  && $(call copy_generated_compiler_file,parser,HaddockLex,x) \
	  && $(call copy_generated_compiler_file,parser,HaddockParse,y) \
	  && $(call copy_generated_compiler_file,parser,Lexer,x) \
	  && $(call copy_generated_compiler_file,parser,Parser,y.pp) \
	  && $(call copy_generated_compiler_file,parser,ParserCore,y) \
	  && $(call copy_generated_util_file,hpc,HpcParser,y) \
	  && $(RM) -rf compiler/stage[123] mk/build.mk \
	  && $(FIND) $(SRC_DIST_DIRS) \( -name _darcs -o -name SRC -o -name "autom4te*" -o -name "*~" -o -name ".cvsignore" -o -name "\#*" -o -name ".\#*" -o -name "log" -o -name "*-SAVE" -o -name "*.orig" -o -name "*.rej" \) -print | xargs $(RM) -rf \
	)
	$(TAR) chf - $(EXTRA_LIBS) | bzip2 >$(FPTOOLS_TOP_ABS)/$(SRC_DIST_EXTRALIBS_TARBALL)
	$(RM) -rf $(EXTRA_LIBS)
	$(TAR) chf - $(SRC_DIST_NAME) 2>$src_log | bzip2 >$(FPTOOLS_TOP_ABS)/$(SRC_DIST_TARBALL)

# Upload the distribution(s)
# Retrying is to work around buggy firewalls that corrupt large file transfers
# over SSH.
ifneq "$(PublishLocation)" ""
dist ::
	@for i in 0 1 2 3 4 5 6 7 8 9; do \
		echo "Try $$i: $(PublishCp) $(SRC_DIST_EXTRALIBS_TARBALL) $(PublishLocation)/dist"; \
		if $(PublishCp) $(SRC_DIST_EXTRALIBS_TARBALL) $(PublishLocation)/dist; then break; fi; \
	done
	@for i in 0 1 2 3 4 5 6 7 8 9; do \
		echo "Try $$i: $(PublishCp) $(SRC_DIST_TARBALL) $(PublishLocation)/dist"; \
		if $(PublishCp) $(SRC_DIST_TARBALL) $(PublishLocation)/dist; then break; fi; \
	done
endif

# -----------------------------------------------------------------------------
# HC file bundles

hc-file-bundle :
	$(RM) -r ghc-$(ProjectVersion)
	$(LN_S) . ghc-$(ProjectVersion)
	$(FIND) ghc-$(ProjectVersion)/compiler \
	     ghc-$(ProjectVersion)/utils \
	     ghc-$(ProjectVersion)/libraries -follow \
	  \( -name "*.hc" -o -name "*_hsc.[ch]" -o -name "*_stub.[ch]" \) -print > hc-files-to-go
	for f in `$(FIND) ghc-$(ProjectVersion)/compiler ghc-$(ProjectVersion)/utils ghc-$(ProjectVersion)/libraries -name "*.hsc" -follow -print` ""; do \
	     if test "x$$f" != "x" && test -f `echo "$$f" | sed 's/hsc$$/hs/g'`; then \
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
	$(TAR) czf ghc-$(ProjectVersion)-$(TARGETPLATFORM)-hc.tar.gz `cat hc-files-to-go`

# -----------------------------------------------------------------------------
# Cleaning

CLEAN_FILES += hc-files-to-go *-hc.tar.gz

DIST_CLEAN_FILES += config.cache config.status mk/config.h mk/stamp-h \
	docs/users_guide/ug-book.xml extra-gcc-opts

# don't clean config.mk: it's needed when cleaning stuff later on
LATE_DIST_CLEAN_FILES += mk/config.mk mk/are-validating.mk

# VERSION is shipped in a source dist
MAINTAINER_CLEAN_FILES += VERSION

extraclean::
	$(RM) -rf autom4te.cache

clean distclean ::
	$(RM) -rf inplace-datadir

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

