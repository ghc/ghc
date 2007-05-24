
# To do a fresh build:
#
#   make clean
#   make boot
#   make
#
# To rebuild a particular library <package>:
#
#   make clean.library.<package>
#   make build.library.<package>
#
# or the following is equivalent:
#
#   make rebuild.library.<package>

.PHONY: default_target

default_target: build

ifeq "$(IN_BIN_DIST)" "1"
include ../Makefile-vars
else
# Ideally we'd just include something to give us variables
# for paths and arguments to tools etc, and those set in mk/build.mk.
TOP=.
include $(TOP)/mk/boilerplate.mk
endif

SUBDIRS = base old-locale old-time directory process filepath pretty \
          template-haskell readline Cabal random haskell98

ifeq "$(GhcLibsWithUnix)" "YES"
SUBDIRS += unix
endif
ifeq "$(Windows)" "YES"
SUBDIRS += $(wildcard Win32)
endif

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
endif

# -----------------------------------------------------------------------------

empty=
space=$(empty) $(empty)

ifeq "$(Windows)" "YES"
dot_bat=.bat
else
dot_bat=
endif

CONFIGURE_OPTS =
CONFIGURE_STAMP_EXTRAS :=

ifneq "$(findstring $(space)p$(space), $(space)$(GhcLibWays)$(space))" ""
CONFIGURE_OPTS += --enable-library-profiling
CONFIGURE_STAMP_EXTRAS := $(CONFIGURE_STAMP_EXTRAS)-profiling
endif

ifeq "$(SplitObjs)" "YES"
CONFIGURE_OPTS += --enable-split-objs
CONFIGURE_STAMP_EXTRAS := $(CONFIGURE_STAMP_EXTRAS)-splitting
endif

BOOTSTRAP_LIBS = Cabal filepath
BOOTSTRAP_STAMPS = $(addprefix stamp/bootstrapping.,$(BOOTSTRAP_LIBS))
BOOTSTRAP_INC_1_UP = $(addprefix -i../bootstrapping.,$(BOOTSTRAP_LIBS))
BOOTSTRAP_INC_2_UP = $(addprefix -i../../bootstrapping.,$(BOOTSTRAP_LIBS))

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

ifneq "$(NO_HADDOCK_DOCS)" "YES"
all: doc
endif

.PHONY: rebuild.library.%

$(foreach SUBDIR,$(SUBDIRS),rebuild.library.$(SUBDIR)):\
rebuild.library.%: clean.library.% build.library.%

build: $(foreach SUBDIR,$(SUBDIRS),build.library.$(SUBDIR))
build: installPackage/installPackage

configure: $(foreach SUBDIR,$(SUBDIRS), \
             stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).$(SUBDIR))

.PHONY: build.library.%

# We should depend on %/%.cabal here (and in other rules), but make
# makes that difficult.

$(foreach SUBDIR,$(SUBDIRS), \
		  stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).$(SUBDIR)): \
stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).%: %/setup/Setup
	-$(RM) -f stamp/configure.library.*.$* $*/unbuildable
	( cd $* && setup/Setup configure \
	           $(CONFIGURE_OPTS) \
	           --prefix=$(prefix) \
	           --with-compiler=../../compiler/ghc-inplace$(dot_bat) \
	           --with-hc-pkg=../../utils/ghc-pkg/ghc-pkg-inplace$(dot_bat) \
	           --with-hsc2hs=../../utils/hsc2hs/hsc2hs-inplace$(dot_bat) \
	           --with-ld=$(LD) \
	           --datasubdir=ghc \
	           --haddock-args="--use-contents=../index.html \
	                           --use-index=../doc-index.html" \
	           $(addprefix --configure-option=,$(CONFIGURE_ARGS)) \
	           --configure-option=--with-cc=$(CC) ) \
	      && touch $@ || touch $*/unbuildable
# We don't touch $@ if configure failed as we would prefer to try
# configuring it next time round, rather than assuming it'll still fail.
# This is particularly important for corelibs, where failure means the
# build dies!

$(foreach SUBDIR,$(SUBDIRS),build.library.$(SUBDIR)):\
build.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
                 %/setup/Setup ifBuildable/ifBuildable
	ifBuildable/ifBuildable $* setup/Setup build \
	                                $(addprefix --ghc-option=,$(GhcLibHcOpts))
	ifBuildable/ifBuildable $* setup/Setup register --inplace

$(foreach SUBDIR,$(SUBDIRS),$(SUBDIR)/CabalMakefile):\
%/CabalMakefile: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
                 %/setup/Setup ifBuildable/ifBuildable
	$(RM) $*/CabalMakefile
	ifBuildable/ifBuildable $* setup/Setup makefile -f CabalMakefile \
	                             $(addprefix --ghc-option=,$(GhcLibHcOpts))

.PHONY: doc

DOC_SUBDIRS = $(filter-out haskell98,$(SUBDIRS))

doc: $(foreach SUBDIR,$(SUBDIRS),doc.library.$(SUBDIR))
	sh gen_contents_index

$(foreach SUBDIR,$(SUBDIRS),doc.library.$(SUBDIR)):\
doc.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
               %/setup/Setup ifBuildable/ifBuildable
	ifBuildable/ifBuildable $* setup/Setup haddock

.PHONY: install install-docs install.library.%

INSTALL_DIR=$(prefix)/share/ghc/doc/html/

install: $(foreach SUBDIR,$(SUBDIRS),install.library.$(SUBDIR))
ifneq "$(NO_HADDOCK_DOCS)" "YES"
	$(INSTALL_DATA)   index.html doc-index.html 			  $(INSTALL_DIR)
	$(INSTALL_SCRIPT) gen_contents_index 					  $(INSTALL_DIR)
	# Hacks:
	$(INSTALL_DATA)   $(prefix)/share/ghc/doc/html/base/*.css $(INSTALL_DIR)
	$(INSTALL_DATA)   $(prefix)/share/ghc/doc/html/base/*.js  $(INSTALL_DIR)
	$(INSTALL_DATA)   $(prefix)/share/ghc/doc/html/base/*.gif $(INSTALL_DIR)
endif

# Cabal doesn't let us ask to install docs only, so do nothing here
install-docs:
	@:

# Ideally this would depend on a stamp/build.library.%, but if it does
# then we can't change the libraries and then just rerun make.
# Thus if you install without building then it will just break.
$(foreach SUBDIR,$(SUBDIRS),install.library.$(SUBDIR)): \
install.library.%: installPackage/installPackage ifBuildable/ifBuildable
	ifBuildable/ifBuildable $* ../installPackage/installPackage

.PHONY: binary-dist binary-dist.library.%

BIN_DIST_LIBDIR=$(BIN_DIST_DIR)/libraries

binary-dist: $(foreach SUBDIR,$(SUBDIRS),binary-dist.library.$(SUBDIR))
	mkdir                               $(BIN_DIST_LIBDIR)/installPackage
	cp 	  installPackage/installPackage $(BIN_DIST_LIBDIR)/installPackage
	cp    Makefile                      $(BIN_DIST_LIBDIR)
	cp    gen_contents_index            $(BIN_DIST_LIBDIR)
	cp    index.html                    $(BIN_DIST_LIBDIR)
	cp    doc-index.html                $(BIN_DIST_LIBDIR)
	cp -a stamp                         $(BIN_DIST_LIBDIR)

$(foreach SUBDIR,$(SUBDIRS),binary-dist.library.$(SUBDIR)): \
binary-dist.library.%:
	$(MKDIRHIER) $(BIN_DIST_LIBDIR)/$*
	ifBuildable/ifBuildable $* cp    $*.cabal      $(BIN_DIST_LIBDIR)/$*
	ifBuildable/ifBuildable $* cp -a dist          $(BIN_DIST_LIBDIR)/$*
	ifBuildable/ifBuildable $* $(FIND) $(BIN_DIST_LIBDIR)/$*/dist \
	     \( -name "*_split" -o -name "autogen" \) | xargs rm -rf
	ifBuildable/ifBuildable $* $(FIND) $(BIN_DIST_LIBDIR)/$*/dist \
	     \( \( -name "*.o" -o -name "*.p_o" \) -a ! -name "HS*" \) \
	     -exec rm {} \;

.PHONY: distclean clean clean.library.%

distclean: clean

clean: $(foreach SUBDIR,$(SUBDIRS),clean.library.$(SUBDIR))
	$(RM) -f stamp/bootstrapping.*
	$(RM) -rf bootstrapping.*
	$(RM) -rf ifBuildable
	$(RM) -f libraries.txt index.html doc-index.html

$(foreach SUBDIR,$(SUBDIRS),clean.library.$(SUBDIR)): \
clean.library.%:
	$(RM) -f stamp/configure.library.*.$* $*/unbuildable
	-cd $* && setup/Setup clean
	$(RM) -rf $*/setup

