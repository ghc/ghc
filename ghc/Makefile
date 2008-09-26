
TOP=..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal-flags.mk

ifeq "$(stage)" ""
stage=1
endif

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALL_FLAGS =
else
INSTALL_FLAGS = --enable-shell-wrappers
endif

# Don't strip the inplace versions, we might need to debug them
INPLACE_INSTALL_FLAGS += --disable-executable-stripping

boot:: boot.stage.$(stage)

all:: build.stage.$(stage)

rebuild ::
	-$(CABAL) clean --distpref dist-stage$(stage)
	$(RM) -rf stage$(stage)-inplace
	$(MAKE) boot all

stage1 ::
	$(MAKE) stage=1

stage2 ::
	$(MAKE) stage=2

stage3 ::
	$(MAKE) stage=3


ifeq "$(CLEAN_ALL_STAGES)" "YES"
clean distclean maintainer-clean:: clean.stage.1 clean.stage.2 clean.stage.3
else
clean distclean maintainer-clean:: clean.stage.$(stage)
endif

ifeq "$(CLEAN_ALL_STAGES)" "YES"
distclean maintainer-clean::
	$(RM) -f ghc-bin.cabal
endif

clean.stage.%:
	$(RM) -rf stage$*-inplace
# This is a bit naughty. We ought to use:
#	-$(CABAL) clean --distpref dist-stage$*
# but that won't work if the Cabal file (a generated file) doesn't
# exist. So we do this instead:
	$(RM) -rf dist-stage$*

CONFIGURE_FLAGS_STAGE1 += --flags=-ghci

ifeq "$(GhcWithInterpreter)" "YES"
CONFIGURE_FLAGS_STAGE2PLUS += --flags=ghci
else
CONFIGURE_FLAGS_STAGE2PLUS += --flags=-ghci
endif

ifeq "$(GhcProfiled)" "YES"
CONFIGURE_FLAGS_STAGE2PLUS += --enable-executable-profiling
endif
ifeq "$(GhcDebugged)" "YES"
CONFIGURE_FLAGS_STAGE2PLUS += --ghc-option=-debug
endif
ifeq "$(GhcThreaded)" "YES"
# Use threaded RTS with GHCi, so threads don't get blocked at the prompt.
CONFIGURE_FLAGS_STAGE2PLUS += --ghc-option=-threaded
endif

ifeq "$(BuildSharedLibs)" "YES"
CONFIGURE_FLAGS_STAGE2PLUS += --ghc-option=-dynamic
endif

CONFIGURE_FLAGS_STAGE2 = $(CONFIGURE_FLAGS_STAGE2PLUS)
CONFIGURE_FLAGS_STAGE3 = $(CONFIGURE_FLAGS_STAGE2PLUS)

CONFIGURE_FLAGS_STAGE1 += $(USE_BOOT_CONFIGURE_FLAGS)
CONFIGURE_FLAGS_STAGE2 += $(USE_STAGE1_CONFIGURE_FLAGS)
CONFIGURE_FLAGS_STAGE3 += $(USE_STAGE2_CONFIGURE_FLAGS)

CONFIGURE_FLAGS_STAGE1 += $(foreach flag, $(GhcStage1HcOpts), --ghc-option=$(flag))
CONFIGURE_FLAGS_STAGE2 += $(foreach flag, $(GhcStage2HcOpts), --ghc-option=$(flag))
CONFIGURE_FLAGS_STAGE3 += $(foreach flag, $(GhcStage3HcOpts), --ghc-option=$(flag))

BUILD_FLAGS += $(patsubst %, --ghc-option=%, $(SRC_HC_OPTS))
# Allow EXTRA_HC_OPTS to be used from the command line to add options 
# when building (e.g. -v)
BUILD_FLAGS += $(patsubst %, --ghc-option=%, $(EXTRA_HC_OPTS))

# XXX In stage2+ we should really use the inplace ghc-pkg
# It works because installPackage doesn't actually use ghc-pkg, as there's
# no library to register

ifeq "$(stage)" "1"
ifeq "$(ghc_ge_609)" "NO"
# GHC 6.9+ can handle cross-package recompilation checking, but
# we don't rely on the bootstrapping compiler being able to do this.
TOUCH_MAIN = $(RM) dist-stage1/build/ghc/ghc-tmp/Main.o
endif
endif

dist-stage1 dist-stage2 dist-stage3: dist-stage%: boot.stage.%

boot.stage.%:
	$(CABAL) configure --distpref dist-stage$*         \
	                   $(INSTALL_DIRS_CONFIGURE_FLAGS) \
	                   $(CONFIGURE_FLAGS_STAGE$*)      \
	                   $(COMMON_CONFIGURE_FLAGS)       \
	                   --libsubdir=.                   \
	                   --datadir='$$libdir'            \
	                   --datasubdir=.

build.stage.%: dist-stage%
	$(TOUCH_MAIN)
	$(CABAL) build     --distpref dist-stage$* $(BUILD_FLAGS)
	$(INSTALL_PACKAGE) install '$(GHC_PKG_PROG)' 'XXX/package.conf' "" \
	                   $(FPTOOLS_TOP_ABS)/ghc/stage$*-inplace          \
	                   $(FPTOOLS_TOP_ABS)/ghc/stage$*-inplace          \
	                   '$$prefix'                                      \
	                   '$(INPLACE_DATA_DIR)'                           \
	                   '$$prefix/libexec'                              \
	                   '$$prefix/dynlib'                               \
	                   '$(INPLACE_DATA_DIR)'                           \
	                   '$$prefix/doc'                                  \
	                   '$$prefix/html'                                 \
	                   '$$prefix/haddock'                              \
	                   --distpref dist-stage$*                         \
	                   $(INPLACE_INSTALL_FLAGS)                        \
	                   $(INSTALL_FLAGS)

# XXX For now we always install the stage 2 compiler
install:
	$(INSTALL_PACKAGE) install UNUSED UNUSED '$(DESTDIR)' '$(prefix)' \
	                   '$(prefix)' '$(bindir)' '$(libdir)'            \
                       '$(libexecdir)' '$(dynlibdir)' '$(datadir)'    \
                       '$(docdir)' '$(htmldir)' '$(haddockdir)'       \
	                   --distpref dist-stage2                         \
	                   $(INSTALL_FLAGS)

html:
	@:

install-docs:
	@:

include $(TOP)/mk/bindist.mk
EXE_DIST_DIR = dist-stage2

