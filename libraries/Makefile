
.PHONY: default_target

default_target: build

ifeq "$(IN_BIN_DIST)" "1"
include ../Makefile-vars
else
TOP=.
include $(TOP)/mk/boilerplate.mk
endif

SUBDIRS = base filepath haskell98 template-haskell readline \
	  regex-base regex-posix regex-compat parsec Cabal

ifeq "$(GhcLibsWithUnix)" "YES"
SUBDIRS += unix
endif
ifeq "$(Windows)" "YES"
SUBDIRS += $(wildcard Win32)
endif

# Set GhcBootLibs=YES from the command line to work with just the libraries
# needed to bootstrap GHC.
ifneq "$(GhcBootLibs)" "YES"
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
ifeq "$(GhcLibsWithObjectIO)" "YES"
SUBDIRS += $(wildcard ObjectIO)
endif
endif

# -----------------------------------------------------------------------------

empty=
space=$(empty) $(empty)

CONFIGURE_OPTS =
CONFIGURE_STAMP_EXTRAS :=

ifneq "$(findstring $(space)p$(space), $(space)$(GhcLibWays)$(space))" ""
CONFIGURE_OPTS += --enable-library-profiling
CONFIGURE_STAMP_EXTRAS := $(CONFIGURE_STAMP_EXTRAS)-profiling
endif

BOOTSTRAPPING_CABAL = bootstrapping.cabal

.PHONY: subdirs

subdirs:
	@echo $(SUBDIRS)

.PHONY: boot

boot: $(foreach SUBDIR,$(SUBDIRS),$(SUBDIR)/setup/Setup)
	# XXX Hideous hacks:
	$(GENPRIMOP) --make-haskell-source < ../compiler/prelude/primops.txt > base/GHC/Prim.hs
	$(GENPRIMOP) --make-haskell-wrappers < ../compiler/prelude/primops.txt > base/GHC/PrimopWrappers.hs

# We build the Setup program in a setup subdirectory to stop it trying
# to use bits of base and Cabal when we build those packages.
# This also makes it slightly easier to clean.

# We ought to be depending on %/Setup.*hs, but make makes that difficult.

$(foreach SUBDIR,$(SUBDIRS),$(SUBDIR)/setup/Setup): \
%/setup/Setup: stamp/$(BOOTSTRAPPING_CABAL)
	-$(RM) -rf $*/setup
	mkdir $*/setup
	$(CP) $*/Setup.*hs $*/setup
	cd $*/setup && $(GHC) --make -i../../$(BOOTSTRAPPING_CABAL) Setup.*hs -o Setup

stamp/$(BOOTSTRAPPING_CABAL):
	$(RM) -rf $(BOOTSTRAPPING_CABAL)
	$(CP) -a Cabal $(BOOTSTRAPPING_CABAL)
	$(FIND) $(BOOTSTRAPPING_CABAL) \( -name "*.o" -o -name "*.hi" \) -exec $(RM) -f {} \;
	touch $@

.PHONY: all build configure

all: build

ifneq "$(NO_HADDOCK_DOCS)" "YES"
all: doc
endif

build: $(foreach SUBDIR,$(SUBDIRS),build.library.$(SUBDIR))

configure: $(foreach SUBDIR,$(SUBDIRS), \
             stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).$(SUBDIR))

.PHONY: build.library.%

# We should depend on %/%.cabal here (and in other rules), but make
# makes that difficult.

$(foreach SUBDIR,$(SUBDIRS), \
		  stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).$(SUBDIR)): \
stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).%: %/setup/Setup
	-$(RM) -f stamp/configure.library.*.$*
	cd $* && setup/Setup configure \
	                     $(CONFIGURE_OPTS) \
	                     --prefix=$(prefix) \
	                     --with-compiler=../../compiler/ghc-inplace \
	                     --with-hc-pkg=../../utils/ghc-pkg/ghc-pkg-inplace \
	                     --datasubdir=ghc \
						 --haddock-args="--use-contents=../index.html --use-index=../doc-index.html"
	touch $@

$(foreach SUBDIR,$(SUBDIRS),build.library.$(SUBDIR)):\
build.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
                 %/setup/Setup
	cd $* && setup/Setup build $(addprefix --ghc-option=,$(GhcLibHcOpts))
	cd $* && setup/Setup register --inplace

.PHONY: doc

DOC_SUBDIRS = $(filter-out haskell98,$(SUBDIRS))

doc: $(foreach SUBDIR,$(SUBDIRS),doc.library.$(SUBDIR))
	sh gen_contents_index

$(foreach SUBDIR,$(SUBDIRS),doc.library.$(SUBDIR)):\
doc.library.%: stamp/configure.library.build$(CONFIGURE_STAMP_EXTRAS).% \
               %/setup/Setup
	cd $* && setup/Setup haddock

.PHONY: install.library.%

INSTALL_DIR=$(prefix)/share/ghc/doc/html/

install: $(foreach SUBDIR,$(SUBDIRS),install.library.$(SUBDIR))
	$(INSTALL_DATA)   index.html doc-index.html 			  $(INSTALL_DIR)
	$(INSTALL_SCRIPT) gen_contents_index 					  $(INSTALL_DIR)
	# Hacks:
	$(INSTALL_DATA)   $(prefix)/share/ghc/doc/html/base/*.css $(INSTALL_DIR)
	$(INSTALL_DATA)   $(prefix)/share/ghc/doc/html/base/*.js  $(INSTALL_DIR)
	$(INSTALL_DATA)   $(prefix)/share/ghc/doc/html/base/*.gif $(INSTALL_DIR)

# Ideally this would depend on a stamp/build.library.%, but if it does
# then we can't change the libraries and then just rerun make.
# Thus if you install without building then it will just break.
$(foreach SUBDIR,$(SUBDIRS),stamp/configure.library.install.$(SUBDIR)): \
stamp/configure.library.install.%: %/setup/Setup
	-$(RM) -f stamp/configure.library.*.$*
	cd $* && setup/Setup configure $(CONFIGURE_OPTS) \
	                               --prefix=$(prefix) \
	                               --with-compiler=$(bindir)/ghc \
	                               --datasubdir=ghc
	touch $@

# We need to reconfigure as we now need to register with the normal ghc-pkg
$(foreach SUBDIR,$(SUBDIRS),install.library.$(SUBDIR)): \
install.library.%: stamp/configure.library.install.% %/setup/Setup
	cd $* && setup/Setup install

.PHONY: clean clean.library.%

clean: $(foreach SUBDIR,$(SUBDIRS),clean.library.$(SUBDIR))
	$(RM) -f stamp/$(BOOTSTRAPPING_CABAL)
	$(RM) -rf $(BOOTSTRAPPING_CABAL)
	$(RM) -f libraries.txt index.html doc-index.html

$(foreach SUBDIR,$(SUBDIRS),clean.library.$(SUBDIR)): \
clean.library.%:
	$(RM) -f stamp/configure.library.*.$*
	-cd $* && setup/Setup clean
	$(RM) -rf $*/setup

