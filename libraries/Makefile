
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
#   make rebuild.library.<package>
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

SUBDIRS  = base array packedstring containers bytestring
SUBDIRS += old-locale old-time filepath directory
ifeq "$(GhcLibsWithUnix)" "YES"
SUBDIRS += unix
endif
ifeq "$(Windows)" "YES"
SUBDIRS += $(wildcard Win32)
endif
SUBDIRS += process pretty hpc template-haskell readline Cabal random haskell98

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
SUBDIRS += $(wildcard X11)
SUBDIRS += $(wildcard time)
ifeq "$(Windows)" "NO"
# HGL is not working on Win32, so omit it for now.  Better not to ship it
# at all than to ship a broken version.
SUBDIRS += $(wildcard HGL)
endif
SUBDIRS += $(wildcard OpenGL)
SUBDIRS += $(wildcard GLUT)
SUBDIRS += $(wildcard OpenAL)
SUBDIRS += $(wildcard ALUT)
SUBDIRS += $(wildcard stm)
SUBDIRS += $(wildcard xhtml)
SUBDIRS += $(wildcard cgi)
SUBDIRS += $(wildcard arrows)
ifeq "$(GhcLibsWithObjectIO)" "YES"
SUBDIRS += $(wildcard ObjectIO)
endif
SUBDIRS += $(wildcard parallel)
SUBDIRS += $(wildcard ndp)
endif

# -----------------------------------------------------------------------------

empty=
space=$(empty) $(empty)

# -----------------------------------------------------------------------------

ifeq "$(RelocatableBuild)" "YES"
# On Windows we want to make moveable bindists, but we need to tell
# ghc-pkg where the haddock docs are. Therefore we completely ignore
# where the user tells us to put the haddock documentation and put it
# somewhere whose relative location we know. When installing we need
# to give Cabal a real path, though.
iprefix     = $$topdir
ibindir     = $$topdir
ilibdir     = $$topdir
ilibexecdir = $$topdir
idatadir    = $$topdir
idocdir     = $$topdir/doc/libraries/$$pkgid
ihtmldir    = $$topdir/doc/libraries/$$pkgid
html_installed_root = $(prefix)/doc/libraries
else
# On non-Windows we can just give absolute paths all the time, and
# thus obey the htmldir that we are given.
iprefix     = $(prefix)
ibindir     = $(bindir)
ilibdir     = $(libdir)
ilibexecdir = $(libexecdir)
idatadir    = $(datadir)
idocdir     = $(docdir)/libraries/$$pkgid
ihtmldir    = $(htmldir)/libraries/$$pkgid
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

BOOTSTRAP_LIBS = Cabal filepath
BOOTSTRAP_STAMPS = $(addprefix stamp/bootstrapping.,$(BOOTSTRAP_LIBS))
BOOTSTRAP_INC_1_UP = -DCABAL_VERSION=1,3 $(addprefix -i../bootstrapping.,$(BOOTSTRAP_LIBS))
BOOTSTRAP_INC_2_UP = -DCABAL_VERSION=1,3 $(addprefix -i../../bootstrapping.,$(BOOTSTRAP_LIBS))

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

$(foreach SUBDIR,$(SUBDIRS),$(SUBDIR)/setup/Setup): \
%/setup/Setup: $(BOOTSTRAP_STAMPS)
	-$(RM) -rf $*/setup
	mkdir $*/setup
	$(CP) $*/Setup.*hs $*/setup
	cd $*/setup && $(GHC) -Wall -cpp --make Setup.*hs -o Setup \
	                      $(BOOTSTRAP_INC_2_UP)

installPackage/installPackage: installPackage.hs $(BOOTSTRAP_STAMPS)
	-$(RM) -rf installPackage
	mkdir installPackage
	$(CP) installPackage.hs installPackage/
	cd installPackage && $(GHC) -Wall -cpp \
	                            --make installPackage -o installPackage \
	                            $(BOOTSTRAP_INC_1_UP)

ifBuildable/ifBuildable: ifBuildable.hs
	-$(RM) -rf ifBuildable
	mkdir ifBuildable
	$(CP) ifBuildable.hs ifBuildable/
	cd ifBuildable && $(GHC) -Wall --make ifBuildable -o ifBuildable

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

$(foreach SUBDIR,$(SUBDIRS),rebuild.library.$(SUBDIR)):\
rebuild.library.%: clean.library.% make.library.%

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

$(foreach SUBDIR,$(SUBDIRS), \
		  stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).$(SUBDIR)): \
stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).%: %/setup/Setup
	-$(RM) -f stamp/configure.library.*.$* $*/unbuildable
	( cd $* && setup/Setup configure \
	           $(CONFIGURE_OPTS) \
	           --prefix=/NONEXISTANT \
	           --bindir=/NONEXISTANT \
	           --libdir=/NONEXISTANT \
	           --libsubdir='$$pkgid' \
	           --libexecdir=/NONEXISTANT \
	           --datadir=/NONEXISTANT \
	           --docdir=/NONEXISTANT \
	           --htmldir=/NONEXISTANT \
	           --with-compiler=../../compiler/stage1/ghc-inplace \
	           --with-hc-pkg=../../utils/ghc-pkg/ghc-pkg-inplace \
	           --with-hsc2hs=../../utils/hsc2hs/hsc2hs-inplace \
	           --with-ld=$(LD) \
	           --haddock-options="--use-contents=../index.html \
	                           --use-index=../doc-index.html" \
	           $(FLAGGED_CONFIGURE_ARGS) \
	           --configure-option=--with-cc=$(CC) ) \
	      && touch $@ || touch $*/unbuildable
# We don't touch $@ if configure failed as we would prefer to try
# configuring it next time round, rather than assuming it'll still fail.
# This is particularly important for bootlibs, where failure means the
# build dies!

# Build the library using 'make'
$(foreach SUBDIR,$(SUBDIRS),make.library.$(SUBDIR)):\
make.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
		%/GNUmakefile \
                %/setup/Setup ifBuildable/ifBuildable
	if ifBuildable/ifBuildable $*; then \
	  cd $* && \
	  $(MAKE) $(MFLAGS) && \
	  setup/Setup register --inplace; \
	fi

# Build the library using 'setup build' (not the default)
$(foreach SUBDIR,$(SUBDIRS),build.library.$(SUBDIR)):\
build.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
                 %/setup/Setup ifBuildable/ifBuildable
	if ifBuildable/ifBuildable $*; then \
	  cd $* && \
	  setup/Setup build $(addprefix --ghc-option=,$(GhcLibHcOpts)); \
	fi

$(foreach SUBDIR,$(SUBDIRS),$(SUBDIR)/GNUmakefile):\
%/GNUmakefile: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
                 %/setup/Setup ifBuildable/ifBuildable
	$(RM) $*/GNUmakefile
	cp Makefile.local $*
	if ifBuildable/ifBuildable $*; then \
	   cd $* && setup/Setup makefile -f GNUmakefile; \
	fi

.PHONY: doc

doc: $(foreach SUBDIR,$(SUBDIRS),doc.library.$(SUBDIR))
	sh gen_contents_index

$(foreach SUBDIR,$(SUBDIRS),doc.library.$(SUBDIR)):\
doc.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
               %/setup/Setup ifBuildable/ifBuildable
	if ifBuildable/ifBuildable $*; then \
	  cd $* && setup/Setup haddock --html-location='../$$pkgid'; \
	fi

.PHONY: distclean clean clean.library.%

distclean: clean

clean: $(foreach SUBDIR,$(SUBDIRS),clean.library.$(SUBDIR))
	$(RM) -f stamp/bootstrapping.*
	$(RM) -rf bootstrapping.*
	$(RM) -rf ifBuildable
	$(RM) -rf installPackage
	$(RM) -f libraries.txt index.html doc-index.html doc-index*.html

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
	$(INSTALL_DATA)   $(DESTDIR)$(html_installed_root)/base*/*.css $(DESTDIR)$(html_installed_root)
	$(INSTALL_DATA)   $(DESTDIR)$(html_installed_root)/base*/*.js  $(DESTDIR)$(html_installed_root)
	$(INSTALL_DATA)   $(DESTDIR)$(html_installed_root)/base*/*.gif $(DESTDIR)$(html_installed_root)
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
	if ifBuildable/ifBuildable $*; then \
	  cd $* && \
	  ../installPackage/installPackage '$(GHC_PKG_PROG)' '$(DESTDIR)$(libdir)/package.conf' '$(DESTDIR)' '$(prefix)' '$(iprefix)' '$(ibindir)' '$(ilibdir)' '$(ilibexecdir)' '$(idatadir)' '$(idocdir)' '$(ihtmldir)' ; \
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
	if ifBuildable/ifBuildable $*; then \
	  $(MKDIRHIER) $(BIN_DIST_LIBDIR)/$*; \
	  cd $* && \
	  cp    $*.cabal      $(BIN_DIST_LIBDIR)/$* && \
	  cp    LICENSE       $(BIN_DIST_LIBDIR)/$* && \
	  cp -R dist          $(BIN_DIST_LIBDIR)/$* && \
	  (cp -RL include      $(BIN_DIST_LIBDIR)/$* || true) && \
	  $(FIND) $(BIN_DIST_LIBDIR)/$*/dist \
	     \( -name "*_split" -o -name "autogen" \) | xargs rm -rf && \
	  $(FIND) $(BIN_DIST_LIBDIR)/$*/dist \
	     \( \( -name "*.o" -o -name "*.p_o" \) -a ! -name "HS*" \) \
	     -exec rm {} \; ; \
	fi
