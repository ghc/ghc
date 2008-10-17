
# To do a fresh build:
#
#   make clean
#   make boot
#   make
#
# To rebuild a particular library <package>:
#
#   make clean.library.<package>
#   make make.library.<package>
#
# or the following is equivalent:
#
#   make remake.library.<package>
#
# To add a new library to the tree, do
#
#   darcs get http://darcs.haskell.org/packages/foo
#   [ -f foo/configure.ac ] && ( cd foo && autoreconf )
#   make make.library.foo

.PHONY: default_target

default_target: all

# make doesn't give us an easy way to get the libraries built in
# dependency order the first time, but not rebuild base (for example)
# when we want to rebuild another library later.
# So for now we just don't do anything in parallel in here.
.NOTPARALLEL:

# Ideally we'd just include something to give us variables
# for paths and arguments to tools etc, and those set in mk/build.mk.
TOP=..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal-flags.mk

# Any libraries listed here should also be in ../packages

SUBDIRS  = ghc-prim $(INTEGER_LIBRARY) base syb base3-compat array packedstring
SUBDIRS += containers bytestring old-locale old-time filepath 
ifeq "$(GhcLibsWithUnix)" "YES"
SUBDIRS += unix
endif
ifeq "$(Windows)" "YES"
SUBDIRS += $(wildcard Win32)
endif
SUBDIRS += directory process pretty hpc template-haskell editline Cabal random haskell98

# Set GhcBootLibs=YES from the command line to work with just the libraries
# needed to bootstrap GHC.
ifneq "$(GhcBootLibs)" "YES"
SUBDIRS += $(wildcard regex-base)
SUBDIRS += $(wildcard regex-posix)
SUBDIRS += $(wildcard regex-compat)
SUBDIRS += $(wildcard parsec)
SUBDIRS += $(wildcard haskell-src)
SUBDIRS += $(wildcard html)
SUBDIRS += $(wildcard network)
SUBDIRS += $(wildcard QuickCheck)
SUBDIRS += $(wildcard HUnit)
SUBDIRS += $(wildcard mtl)
SUBDIRS += $(wildcard time)
SUBDIRS += $(wildcard stm)
SUBDIRS += $(wildcard xhtml)
SUBDIRS += $(wildcard parallel)
ifneq "$(wildcard dph)" ""
SUBDIRS += dph/dph-base
SUBDIRS += dph/dph-prim-interface
SUBDIRS += dph/dph-prim-seq
SUBDIRS += dph/dph-prim-par
SUBDIRS += dph/dph-seq
SUBDIRS += dph/dph-par
endif
endif

# -----------------------------------------------------------------------------

empty=
space=$(empty) $(empty)

# -----------------------------------------------------------------------------

ifeq "$(ghc_ge_609)" "YES"
GhcLibHcOpts += -fno-warn-deprecated-flags
endif

IFBUILDABLE=ifBuildable/ifBuildable $(FPTOOLS_TOP_ABS)/packages

ifneq "$(DOING_BIN_DIST)" "YES"

CONFIGURE_OPTS =
CONFIGURE_STAMP_EXTRAS :=

ifneq "$(findstring $(space)p$(space), $(space)$(GhcLibWays)$(space))" ""
CONFIGURE_OPTS += --enable-library-profiling
CONFIGURE_STAMP_EXTRAS := $(CONFIGURE_STAMP_EXTRAS)-profiling
endif

ifeq "$(BuildSharedLibs)" "YES"
CONFIGURE_OPTS += --enable-shared
CONFIGURE_STAMP_EXTRAS := $(CONFIGURE_STAMP_EXTRAS)-shared
endif

ifeq "$(SplitObjs)" "YES"
CONFIGURE_OPTS += --enable-split-objs
CONFIGURE_STAMP_EXTRAS := $(CONFIGURE_STAMP_EXTRAS)-splitting
endif

.PHONY: subdirs

subdirs:
	@echo $(SUBDIRS)

HERE_ABS=$(FPTOOLS_TOP_ABS)/libraries

CABAL_GHC_FLAGS = -Wall
ifeq "$(ghc_ge_605)" "NO"
CABAL_GHC_FLAGS += -cpp
endif

BOOTSTRAPPING_FLAGS = $(CABAL_GHC_FLAGS) -DCABAL_VERSION=$(CABAL_VERSION) -odir $(HERE_ABS)/bootstrapping -hidir $(HERE_ABS)/bootstrapping -i$(HERE_ABS)/Cabal -i$(HERE_ABS)/filepath -i$(HERE_ABS)/hpc

.PHONY: boot

boot: ifBuildable/ifBuildable cabal-bin bootstrapping.conf

cabal-bin: cabal-bin.hs
	-mkdir bootstrapping
	$(GHC) $(BOOTSTRAPPING_FLAGS) --make cabal-bin -o cabal-bin

bootstrapping.conf: cabal-bin
	echo "[]" > $@.tmp
	-cd extensible-exceptions && $(CABAL) clean     --distpref=dist-bootstrapping
	cd extensible-exceptions && $(CABAL) configure --distpref=dist-bootstrapping --with-compiler=$(GHC) --with-hc-pkg=$(GHC_PKG) --package-db=$(HERE_ABS)/$@.tmp
	cd extensible-exceptions && $(CABAL) build     --distpref=dist-bootstrapping
	cd extensible-exceptions && $(CABAL) install   --distpref=dist-bootstrapping --inplace
	-cd filepath && $(CABAL) clean     --distpref=dist-bootstrapping
	cd filepath && $(CABAL) configure --distpref=dist-bootstrapping --with-compiler=$(GHC) --with-hc-pkg=$(GHC_PKG) --package-db=$(HERE_ABS)/$@.tmp
	cd filepath && $(CABAL) build     --distpref=dist-bootstrapping
	cd filepath && $(CABAL) install   --distpref=dist-bootstrapping --inplace
	-cd Cabal    && $(CABAL) clean     --distpref=dist-bootstrapping
	cd Cabal    && $(CABAL) configure --distpref=dist-bootstrapping --with-compiler=$(GHC) --with-hc-pkg=$(GHC_PKG) --package-db=$(HERE_ABS)/$@.tmp
	cd Cabal    && $(CABAL) build     --distpref=dist-bootstrapping
	cd Cabal    && $(CABAL) install   --distpref=dist-bootstrapping --inplace
	-cd hpc    && $(CABAL) clean     --distpref=dist-bootstrapping
	cd hpc    && $(CABAL) configure --distpref=dist-bootstrapping --with-compiler=$(GHC) --with-hc-pkg=$(GHC_PKG) --package-db=$(HERE_ABS)/$@.tmp
	cd hpc    && $(CABAL) build     --distpref=dist-bootstrapping
	cd hpc    && $(CABAL) install   --distpref=dist-bootstrapping --inplace
	mv $@.tmp $@

ifBuildable/ifBuildable: ifBuildable.hs
	-$(RM) -rf ifBuildable
	mkdir ifBuildable
	$(CP) ifBuildable.hs ifBuildable/
ifeq "$(stage)" "2"
	cd ifBuildable && ../$(HC) -Wall --make ifBuildable -o ifBuildable
else
	cd ifBuildable && $(GHC) -Wall --make ifBuildable -o ifBuildable
endif

.PHONY: all build configure

all: build

.PHONY: rebuild.library.%
.PHONY: remake.library.%

$(foreach SUBDIR,$(SUBDIRS),rebuild.library.$(SUBDIR)):\
rebuild.library.%: clean.library.% build.library.%

$(foreach SUBDIR,$(SUBDIRS),remake.library.$(SUBDIR)):\
remake.library.%: clean.library.% make.library.%

# NB. we're depending on make chasing dependencies from left to right here.
# This bit goes wrong with 'make -j'.
build: $(foreach SUBDIR,$(SUBDIRS),make.library.$(SUBDIR))

configure: $(foreach SUBDIR,$(SUBDIRS), \
             stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).$(SUBDIR))

.PHONY: build.library.%
.PHONY: make.library.%

# We should depend on %/%.cabal here (and in other rules), but make
# makes that difficult.

ALL_CONFIGURE_FLAGS = \
               $(INSTALL_DIRS_CONFIGURE_FLAGS) \
               $(USE_STAGE1_CONFIGURE_FLAGS) \
               $(COMMON_CONFIGURE_FLAGS) \
	           --haddock-options="--use-contents=../index.html \
	                              --use-index=../doc-index.html" \
	           $(CONFIGURE_OPTS)

stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).dph/dph-par: \
    dph/dph-par

stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).dph/dph-seq: \
    dph/dph-seq

dph/%:
	$(MAKE) -C dph $*

$(foreach SUBDIR,$(SUBDIRS), \
		  stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).$(SUBDIR)): \
stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).%: cabal-bin
	-$(RM) -f stamp/configure.library.*.$* $*/unbuildable
	$(MKDIRHIER) `dirname $@`
	( cd $* && $(CABAL) configure $(ALL_CONFIGURE_FLAGS) ) \
	      && touch $@ || touch $*/unbuildable
# We don't touch $@ if configure failed as we would prefer to try
# configuring it next time round, rather than assuming it'll still fail.
# This is particularly important for bootlibs, where failure means the
# build dies!

# Build the library using 'make'
# We re-run 'setup makefile' each time, just in case any preprocessing
# needs to be done.  However, we're careful not to overwrite GNUmakefile
# if it hasn't changed, so that dependency-generation isn't forced
# every time.
$(foreach SUBDIR,$(SUBDIRS),make.library.$(SUBDIR)):\
make.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
                cabal-bin ifBuildable/ifBuildable
	if $(IFBUILDABLE) $*; then \
	  cd $* && \
	  cmp -s $(MAKEFILE_LOCAL) Makefile.local || cp $(MAKEFILE_LOCAL) .; \
	  mv GNUmakefile GNUmakefile.tmp; \
	  $(CABAL) makefile -f GNUmakefile; \
	  cmp -s GNUmakefile GNUmakefile.tmp && mv GNUmakefile.tmp GNUmakefile; \
	  $(MAKE) $(MFLAGS) && \
	  $(CABAL) register --inplace; \
	fi

MAKEFILE_LOCAL = $(FPTOOLS_TOP_ABS)/libraries/Makefile.local

# Build the library using 'setup build' (not the default)
$(foreach SUBDIR,$(SUBDIRS),build.library.$(SUBDIR)):\
build.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
                 cabal-bin ifBuildable/ifBuildable
	if $(IFBUILDABLE) $*; then \
	  cd $* && \
	  $(CABAL) build $(BUILD_FLAGS); \
	  $(CABAL) register --inplace; \
	fi

.PHONY: doc html

html: doc

# No docs for compat libraries for now.
DOC_SUBDIRS = $(filter-out %-compat, $(SUBDIRS))

doc: $(foreach SUBDIR,$(DOC_SUBDIRS),doc.library.$(SUBDIR))
	sh gen_contents_index --inplace

# Making hyperlinked source only works if we have hscolour
ifneq "$(HSCOLOUR)" ""
CABAL_HADDOCK_FLAGS += --hyperlink-source
endif
CABAL_HADDOCK_FLAGS += --with-haddock=$(FPTOOLS_TOP_ABS)/utils/haddock/install-inplace/bin/haddock

$(foreach SUBDIR,$(DOC_SUBDIRS),doc.library.$(SUBDIR)):\
doc.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
               cabal-bin ifBuildable/ifBuildable
	if $(IFBUILDABLE) $*; then \
	  cd $* && $(CABAL) haddock --html-location='../$$pkg' \
	                            $(CABAL_HADDOCK_FLAGS); \
	fi
ifneq "$(HSCOLOUR)" ""
# We use */src rather than $*/src due to the $(INTEGER_LIBRARY)/integer
# mismatch
# XXX We also need to check that the directory exists, as e.g. dph disables
# haddock, so the directory doesn't get made. We should remove this once
# we can always haddock everything.
	if $(IFBUILDABLE) $* && [ -d $*/dist/doc/html/*/src/ ]; then cp hscolour.css $*/dist/doc/html/*/src/; fi
endif

.PHONY: maintainer-clean distclean clean clean.library.%

maintainer-clean: clean
distclean: clean

clean: $(foreach SUBDIR,$(SUBDIRS),clean.library.$(SUBDIR))
	-cd extensible-exceptions && $(CABAL) clean --distpref=dist-bootstrapping
	-cd filepath              && $(CABAL) clean --distpref=dist-bootstrapping
	-cd Cabal                 && $(CABAL) clean --distpref=dist-bootstrapping
	-cd hpc                   && $(CABAL) clean --distpref=dist-bootstrapping
	$(RM) -rf bootstrapping
	$(RM) -f bootstrapping.conf     bootstrapping.conf.old
	$(RM) -f bootstrapping.conf.tmp bootstrapping.conf.tmp.old
	$(RM) -f cabal-bin cabal-bin.exe
	$(RM) -rf ifBuildable
	$(RM) -f libraries.txt index.html doc-index.html doc-index*.html
	$(RM) -f haddock* *.gif
	$(RM) -rf stamp/*
ifneq "$(wildcard dph)" ""
	$(MAKE) -C dph clean
endif

maintainer-clean distclean:
	$(RM) $(foreach lib, $(SUBDIRS), $(lib)/.depend $(lib)/.depend.bak)

$(foreach SUBDIR,$(SUBDIRS),clean.library.$(SUBDIR)): \
clean.library.%:
	$(RM) -f stamp/configure.library.*.$* $*/unbuildable
	-cd $* && $(CABAL) clean
	$(RM) -f $*/Setup $*/Setup.exe $*/Setup.hi $*/Setup.o
	$(RM) $*/GNUmakefile $*/Makefile.local
endif

# -----------------------------------------------------------------------------

.PHONY: install install-docs install.library.%

install: $(foreach SUBDIR,$(SUBDIRS),install.library.$(SUBDIR))
ifeq "$(HADDOCK_DOCS)" "YES"
	$(INSTALL_DIR)                                                     $(DESTDIR)$(html_installed_root)
	$(INSTALL_DATA)   index.html doc-index*.html                       $(DESTDIR)$(html_installed_root)
	$(INSTALL_SCRIPT) gen_contents_index                               $(DESTDIR)$(html_installed_root)
	$(INSTALL_DATA)   prologue.txt                                     $(DESTDIR)$(html_installed_root)
	# Hacks:
	$(INSTALL_DATA)   base/dist/doc/html/*/*.css $(DESTDIR)$(html_installed_root)
	$(INSTALL_DATA)   base/dist/doc/html/*/*.js $(DESTDIR)$(html_installed_root)
	$(INSTALL_DATA)   base/dist/doc/html/*/*.gif $(DESTDIR)$(html_installed_root)
endif

# Cabal doesn't let us ask to install docs only, so do nothing here
install-docs:
	@:

# Ideally this would depend on a stamp/build.library.%, but if it does
# then we can't change the libraries and then just rerun make.
# Thus if you install without building then it will just break.

# prefix and (on Windows) htmldir use $topdir when configuring, so we
# need to tell installPackage the real path to use when installing.

# We also need to pass all the other directories in, as they can be
# overridden when installing a bindist.

$(foreach SUBDIR,$(SUBDIRS),install.library.$(SUBDIR)): \
install.library.%: ifBuildable/ifBuildable
	if $(IFBUILDABLE) $*; then \
	  cd $* && \
	  $(INSTALL_PACKAGE) install '$(GHC_PKG_INSTALL_PROG)' '$(DESTDIR)$(datadir)/package.conf' '$(DESTDIR)' '$(prefix)' '$(iprefix)' '$(ibindir)' '$(ilibdir)' '$(ilibexecdir)' '$(idynlibdir)' '$(idatadir)' '$(idocdir)' '$(ihtmldir)' '$(ihaddockdir)' ; \
	fi

.PHONY: binary-dist binary-dist.library.%

BIN_DIST_LIBDIR=$(BIN_DIST_DIR)/libraries

binary-dist: $(foreach SUBDIR,$(SUBDIRS),binary-dist.library.$(SUBDIR))
ifeq "$(WHERE_AM_I)" ""
	echo "I don't know where I am" >&2
	exit 1
endif
	echo $(WHERE_AM_I)/Makefile >> $(BIN_DIST_LIST)
# XXX This needs to be changed: This ifBuildable is built with the
# bootstrapping compiler, so isn't OS X friendly. It should be made into
# a Cabal package if we keep it. However, once we drop extralibs we can
# probably remove it anyway.
	echo $(WHERE_AM_I)/ifBuildable/ifBuildable >> $(BIN_DIST_LIST)
	for FILE in dph/dph-prim-interface/interface/*.h dph/dph/LICENSE; do if [ -f $$FILE ]; then echo $(WHERE_AM_I)/$$FILE >> $(BIN_DIST_LIST); fi; done
ifeq "$(HADDOCK_DOCS)" "YES"
	for FILE in gen_contents_index prologue.txt index.html doc-index*.html; do echo $(WHERE_AM_I)/$$FILE >> $(BIN_DIST_LIST); done
endif

$(foreach SUBDIR,$(SUBDIRS),binary-dist.library.$(SUBDIR)): \
binary-dist.library.%:
	if $(IFBUILDABLE) $*; then \
	  $(MAKE) -C $* -f Makefile.local binary-dist BINDIST_EXTRAS="$*.cabal LICENSE $*.buildinfo unbuildable" WHERE_AM_I=$(WHERE_AM_I)/$*; \
	fi

# Ignore some doc targets that we don't support
# The root recurses into us when these targets are made
.PHONY: html-no-chunks chm HxS fo dvi ps pdf
html-no-chunks chm HxS fo dvi ps pdf:
	@:

