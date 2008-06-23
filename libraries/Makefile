
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
#   [ -e foo/configure.ac ] && ( cd foo && autoreconf )
#   make make.library.foo

.PHONY: default_target show

default_target: all

show:
	@echo '$(VALUE)="$($(VALUE))"'

# make doesn't give us an easy way to get the libraries built in
# dependency order the first time, but not rebuild base (for example)
# when we want to rebuild another library later.
# So for now we just don't do anything in parallel in here.
.NOTPARALLEL:

# Ideally we'd just include something to give us variables
# for paths and arguments to tools etc, and those set in mk/build.mk.
TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS  = ghc-prim $(INTEGER_LIBRARY) base array packedstring
SUBDIRS += containers bytestring old-locale old-time filepath directory
ifeq "$(GhcLibsWithUnix)" "YES"
SUBDIRS += unix
endif
ifeq "$(Windows)" "YES"
SUBDIRS += $(wildcard Win32)
endif
SUBDIRS += process pretty hpc template-haskell editline Cabal random haskell98

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
SUBDIRS += $(wildcard fgl)
SUBDIRS += $(wildcard time)
SUBDIRS += $(wildcard OpenGL)
SUBDIRS += $(wildcard GLUT)
SUBDIRS += $(wildcard OpenAL)
SUBDIRS += $(wildcard ALUT)
SUBDIRS += $(wildcard stm)
SUBDIRS += $(wildcard xhtml)
SUBDIRS += $(wildcard cgi)
ifeq "$(GhcLibsWithObjectIO)" "YES"
SUBDIRS += $(wildcard ObjectIO)
endif
SUBDIRS += $(wildcard parallel)
SUBDIRS_BUILD += $(wildcard ndp)
SUBDIRS_BUILD += $(wildcard dph/dph-base)
SUBDIRS_BUILD += $(wildcard dph/dph-prim-seq)
SUBDIRS_BUILD += $(wildcard dph/dph-prim-par)
SUBDIRS_BUILD += $(wildcard dph/dph)
endif

SUBDIRS += $(SUBDIRS_BUILD)

# -----------------------------------------------------------------------------

empty=
space=$(empty) $(empty)

# -----------------------------------------------------------------------------

ifeq "$(ghc_ge_609)" "YES"
GhcLibHcOpts += -fno-warn-deprecated-flags
endif

ifeq "$(RelocatableBuild)" "YES"
# On Windows we want to make moveable bindists, but we need to tell
# ghc-pkg where the haddock docs are. Therefore we completely ignore
# where the user tells us to put the haddock documentation and put it
# somewhere whose relative location we know. When installing we need
# to give Cabal a real path, though.
iprefix             = $$topdir
ibindir             = $$topdir
ilibdir             = $$topdir
ilibexecdir         = $$topdir
idynlibdir          = $$topdir
idatadir            = $$topdir
idocdir             = $$topdir/doc/libraries/$$pkg
ihaddockdir         = $$topdir/doc/libraries/$$pkg
ihtmldir            = $$httptopdir/doc/libraries/$$pkg
html_installed_root = $(prefix)/doc/libraries
else
# On non-Windows we can just give absolute paths all the time, and
# thus obey the htmldir that we are given.
iprefix             = $(prefix)
ibindir             = $(bindir)
ilibdir             = $(libdir)
ilibexecdir         = $(libexecdir)
idynlibdir          = $(dynlibdir)
idatadir            = $(datadir)
idocdir             = $(docdir)/libraries/$$pkg
ihaddockdir         = $(htmldir)/libraries/$$pkg
ihtmldir            = $(htmldir)/libraries/$$pkg
html_installed_root = $(htmldir)/libraries
endif

ifneq "$(DOING_BIN_DIST)" "YES"

CONFIGURE_OPTS =
CONFIGURE_STAMP_EXTRAS :=

ifneq "$(findstring $(space)p$(space), $(space)$(GhcLibWays)$(space))" ""
CONFIGURE_OPTS += --enable-library-profiling
CONFIGURE_STAMP_EXTRAS := $(CONFIGURE_STAMP_EXTRAS)-profiling
endif

ifneq "$(findstring $(space)dyn$(space), $(space)$(GhcLibWays)$(space))" ""
CONFIGURE_OPTS += --enable-shared
CONFIGURE_STAMP_EXTRAS := $(CONFIGURE_STAMP_EXTRAS)-shared
endif

ifeq "$(SplitObjs)" "YES"
CONFIGURE_OPTS += --enable-split-objs
CONFIGURE_STAMP_EXTRAS := $(CONFIGURE_STAMP_EXTRAS)-splitting
endif

ifneq "$(HSCOLOUR)" ""
CONFIGURE_OPTS += --with-hscolour=$(HSCOLOUR)
endif

BOOTSTRAP_LIBS = Cabal filepath
BOOTSTRAP_STAMPS = $(addprefix stamp/bootstrapping.,$(BOOTSTRAP_LIBS))
BOOTSTRAP_INC = -DCABAL_VERSION=1,3 $(addprefix -i$(FPTOOLS_TOP_ABS)/libraries/bootstrapping.,$(BOOTSTRAP_LIBS))
DEPLOYMENT_OPTS = $(addprefix -optc, $(MACOSX_DEPLOYMENT_CC_OPTS)) \
		  $(addprefix -optl, $(MACOSX_DEPLOYMENT_LD_OPTS))

.PHONY: subdirs

subdirs:
	@echo $(SUBDIRS)

.PHONY: boot

boot: $(BOOTSTRAP_STAMPS) ifBuildable/ifBuildable \
	  $(foreach SUBDIR,$(SUBDIRS),$(SUBDIR)/setup/Setup) \
	  installPackage/installPackage

# We build the Setup program in a setup subdirectory to stop it trying
# to use bits of base and Cabal when we build those packages.
# This also makes it slightly easier to clean.

# We ought to be depending on %/Setup.*hs, but make makes that difficult.

CABAL_GHC_FLAGS = -Wall
ifeq "$(ghc_ge_605)" "NO"
CABAL_GHC_FLAGS += -cpp
endif

$(foreach SUBDIR,$(SUBDIRS),$(SUBDIR)/setup/Setup): \
%/setup/Setup: $(BOOTSTRAP_STAMPS)
	-$(RM) -rf $*/setup
	mkdir $*/setup
	$(CP) $*/Setup.*hs $*/setup
	cd $*/setup && $(GHC) $(CABAL_GHC_FLAGS) --make Setup.*hs -o Setup \
	                      $(BOOTSTRAP_INC)

installPackage/installPackage: installPackage.hs $(BOOTSTRAP_STAMPS)
	-$(RM) -rf installPackage
	mkdir installPackage
	$(CP) installPackage.hs installPackage/
ifeq "$(stage)" "2"
	cd installPackage && ../$(HC) $(CABAL_GHC_FLAGS) \
	                            --make installPackage -o installPackage \
	                            $(BOOTSTRAP_INC) $(DEPLOYMENT_OPTS)
else
	cd installPackage && $(GHC) $(CABAL_GHC_FLAGS) \
	                            --make installPackage -o installPackage \
	                            $(BOOTSTRAP_INC)
endif
# TODO: The conditional is yucky, but we need to be able to build with stage1
#       for bindists and as installPackages/ has no Makefile, it seems hard
#	to make this nicer.

ifBuildable/ifBuildable: ifBuildable.hs
	-$(RM) -rf ifBuildable
	mkdir ifBuildable
	$(CP) ifBuildable.hs ifBuildable/
ifeq "$(stage)" "2"
	cd ifBuildable && ../$(HC) -Wall --make ifBuildable -o ifBuildable \
	   	       	         $(DEPLOYMENT_OPTS)
else
	cd ifBuildable && $(GHC) -Wall --make ifBuildable -o ifBuildable
endif

IFBUILDABLE=ifBuildable/ifBuildable $(FPTOOLS_TOP_ABS)/libraries/boot-packages

$(BOOTSTRAP_STAMPS): stamp/bootstrapping.%:
	$(RM) -rf bootstrapping.$*
	$(CP) -R $* bootstrapping.$*
	$(FIND) bootstrapping.$* \( -name "*.o" -o -name "*.hi" \) \
	                         -exec $(RM) -f {} \;
	touch $@

.PHONY: all build configure

all: build

ifeq "$(HADDOCK_DOCS)" "YES"
all: doc
endif

.PHONY: rebuild.library.%
.PHONY: remake.library.%

$(foreach SUBDIR,$(SUBDIRS),rebuild.library.$(SUBDIR)):\
rebuild.library.%: clean.library.% build.library.%

$(foreach SUBDIR,$(SUBDIRS),remake.library.$(SUBDIR)):\
remake.library.%: clean.library.% make.library.%

# NB. we're depending on make chasing dependencies from left to right here.
# This bit goes wrong with 'make -j'.
build: $(foreach SUBDIR,$(SUBDIRS),make.library.$(SUBDIR))
build: installPackage/installPackage

configure: $(foreach SUBDIR,$(SUBDIRS), \
             stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).$(SUBDIR))

.PHONY: build.library.%
.PHONY: make.library.%

# We should depend on %/%.cabal here (and in other rules), but make
# makes that difficult.

# We put non-existant paths in when configuring, as we require that
# builds don't depend on these paths when making bindists.

# We rely on all the CONFIGURE_ARGS being quoted with '...', and there
# being no 's inside the values.
FLAGGED_CONFIGURE_ARGS = $(subst $(space)',\
                                 $(space)--configure-option=',\
                                 $(space)$(CONFIGURE_ARGS))

ifeq "$(Windows)" "YES"
NONEXISTENT=c:/NONEXISTENT
else
NONEXISTENT=/NONEXISTENT
endif

ALL_CONFIGURE_FLAGS = \
	           $(CONFIGURE_OPTS) \
	           --prefix=$(NONEXISTENT) \
	           --bindir=$(NONEXISTENT) \
	           --libdir=$(NONEXISTENT) \
	           --libsubdir='$$pkgid' \
	           --libexecdir=$(NONEXISTENT) \
	           --datadir=$(NONEXISTENT) \
	           --docdir=$(NONEXISTENT) \
			   --haddockdir=$(NONEXISTENT) \
	           --htmldir=$(NONEXISTENT) \
	           --with-compiler=$(FPTOOLS_TOP_ABS)/compiler/stage1/ghc-inplace \
	           --with-hc-pkg=$(FPTOOLS_TOP_ABS)/utils/ghc-pkg/ghc-pkg-inplace \
	           --with-hsc2hs=$(FPTOOLS_TOP_ABS)/utils/hsc2hs/hsc2hs-inplace \
	           --with-ld=$(LD) \
	           --haddock-options="--use-contents=../index.html \
	                              --use-index=../doc-index.html" \
	           $(FLAGGED_CONFIGURE_ARGS) \
	           --configure-option=--with-cc=$(CC)

$(foreach SUBDIR,$(SUBDIRS), \
		  stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).$(SUBDIR)): \
stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).%: %/setup/Setup
	-$(RM) -f stamp/configure.library.*.$* $*/unbuildable
	$(MKDIRHIER) `dirname $@`
	( cd $* && setup/Setup configure $(ALL_CONFIGURE_FLAGS) ) \
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
# Packages in $(SUBDIRS_BUILD) need to use the build.* rules instead of
# make.*, so filter them out and use an alternate rule below instead.
$(foreach SUBDIR,$(filter-out $(SUBDIRS_BUILD),$(SUBDIRS)),make.library.$(SUBDIR)):\
make.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
                %/setup/Setup ifBuildable/ifBuildable installPackage/installPackage
	if $(IFBUILDABLE) $*; then \
	  cd $* && \
	  cmp -s $(MAKEFILE_LOCAL) Makefile.local || cp $(MAKEFILE_LOCAL) .; \
	  mv GNUmakefile GNUmakefile.tmp; \
	  setup/Setup makefile -f GNUmakefile; \
	  cmp -s GNUmakefile GNUmakefile.tmp && mv GNUmakefile.tmp GNUmakefile; \
	  $(MAKE) $(MFLAGS) && \
	  setup/Setup register --inplace; \
	fi

MAKEFILE_LOCAL = $(FPTOOLS_TOP_ABS)/libraries/Makefile.local

# Alternate rule, as described above
$(foreach SUBDIR,$(SUBDIRS_BUILD),make.library.$(SUBDIR)):\
make.library.%: build.library.%

# Build the library using 'setup build' (not the default)
$(foreach SUBDIR,$(SUBDIRS),build.library.$(SUBDIR)):\
build.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
                 %/setup/Setup ifBuildable/ifBuildable
	if $(IFBUILDABLE) $*; then \
	  cd $* && \
	  setup/Setup build $(addprefix --ghc-option=,$(GhcLibHcOpts)); \
	  setup/Setup register --inplace; \
	fi

.PHONY: doc html

html: doc

doc: $(foreach SUBDIR,$(SUBDIRS),doc.library.$(SUBDIR))
	sh gen_contents_index --inplace

# Making hyperlinked source only works if we have hscolour
ifneq "$(HSCOLOUR)" ""
CABAL_HADDOCK_FLAGS=--hyperlink-source
endif

$(foreach SUBDIR,$(SUBDIRS),doc.library.$(SUBDIR)):\
doc.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
               %/setup/Setup ifBuildable/ifBuildable
	if $(IFBUILDABLE) $*; then \
	  cd $* && setup/Setup haddock --html-location='../$$pkg' \
	                               $(CABAL_HADDOCK_FLAGS); \
	fi
ifneq "$(HSCOLOUR)" ""
# We use */src rather than $*/src due to the $(INTEGER_LIBRARY)/integer
# mismatch
	if $(IFBUILDABLE) $*; then cp hscolour.css $*/dist/doc/html/*/src/; fi
endif

.PHONY: distclean clean clean.library.%

distclean: clean

clean: $(foreach SUBDIR,$(SUBDIRS),clean.library.$(SUBDIR))
	$(RM) -f stamp/bootstrapping.*
	$(RM) -rf bootstrapping.*
	$(RM) -rf ifBuildable
	$(RM) -rf installPackage
	$(RM) -f libraries.txt index.html doc-index.html doc-index*.html
	$(RM) -f haddock* *.gif

distclean:
	$(RM) $(foreach lib, $(SUBDIRS), $(lib)/.depend $(lib)/.depend.bak)

$(foreach SUBDIR,$(SUBDIRS),clean.library.$(SUBDIR)): \
clean.library.%:
	$(RM) -f stamp/configure.library.*.$* $*/unbuildable
	-cd $* && setup/Setup clean
	$(RM) -rf $*/setup
	$(RM) $*/GNUmakefile $*/Makefile.local
endif

# -----------------------------------------------------------------------------

.PHONY: install install-docs install.library.%

install: $(foreach SUBDIR,$(SUBDIRS),install.library.$(SUBDIR))
ifeq "$(HADDOCK_DOCS)" "YES"
	$(INSTALL_DIR)                                                     $(DESTDIR)$(html_installed_root)
	$(INSTALL_DATA)   index.html doc-index*.html                       $(DESTDIR)$(html_installed_root)
	$(INSTALL_SCRIPT) gen_contents_index                               $(DESTDIR)$(html_installed_root)
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
install.library.%: installPackage/installPackage ifBuildable/ifBuildable
	if $(IFBUILDABLE) $*; then \
	  cd $* && \
	  $(FPTOOLS_TOP_ABS)/libraries/installPackage/installPackage install '$(GHC_PKG_PROG)' '$(DESTDIR)$(libdir)/package.conf' '$(DESTDIR)' '$(prefix)' '$(iprefix)' '$(ibindir)' '$(ilibdir)' '$(ilibexecdir)' '$(idynlibdir)' '$(idatadir)' '$(idocdir)' '$(ihtmldir)' '$(ihaddockdir)' ; \
	fi

.PHONY: binary-dist binary-dist.library.%

BIN_DIST_LIBDIR=$(BIN_DIST_DIR)/libraries

binary-dist: $(foreach SUBDIR,$(SUBDIRS),binary-dist.library.$(SUBDIR))
	mkdir                               $(BIN_DIST_LIBDIR)/installPackage
	cp 	  installPackage/installPackage $(BIN_DIST_LIBDIR)/installPackage
	mkdir                               $(BIN_DIST_LIBDIR)/ifBuildable
	cp 	  ifBuildable/ifBuildable       $(BIN_DIST_LIBDIR)/ifBuildable
	cp    Makefile                      $(BIN_DIST_LIBDIR)
ifeq "$(HADDOCK_DOCS)" "YES"
	cp    gen_contents_index            $(BIN_DIST_LIBDIR)
	cp    index.html                    $(BIN_DIST_LIBDIR)
	cp    doc-index*.html               $(BIN_DIST_LIBDIR)
endif
	cp -pR stamp                         $(BIN_DIST_LIBDIR)

$(foreach SUBDIR,$(SUBDIRS),binary-dist.library.$(SUBDIR)): \
binary-dist.library.%:
	if $(IFBUILDABLE) $*; then \
	  $(MKDIRHIER) $(BIN_DIST_LIBDIR)/$*; \
	  cd $* && \
	  cp    $*.cabal      $(BIN_DIST_LIBDIR)/$* && \
	  cp    LICENSE       $(BIN_DIST_LIBDIR)/$* && \
	  cp -R dist          $(BIN_DIST_LIBDIR)/$* && \
	  $(FIND) . -name "*.buildinfo" -exec cp {} $(BIN_DIST_LIBDIR)/$* \; && \
	  $(FIND) . -name unbuildable -exec cp {} $(BIN_DIST_LIBDIR)/$* \; && \
	  (cp -RL include      $(BIN_DIST_LIBDIR)/$* || true) && \
	  $(FIND) $(BIN_DIST_LIBDIR)/$*/dist \
	     \( -name "*_split" -o -name "autogen" \) | xargs rm -rf && \
	  $(FIND) $(BIN_DIST_LIBDIR)/$*/dist \
	     \( \( -name "*.o" -o -name "*.p_o" \) -a ! -name "HS*" \) \
	     -exec rm {} \; ; \
	fi

# Ignore some doc targets that we don't support
# The root recurses into us when these targets are made
.PHONY: html-no-chunks chm HxS fo dvi ps pdf
html-no-chunks chm HxS fo dvi ps pdf:
	@:

