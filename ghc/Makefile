
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

boot:: boot.stage.$(stage)

all:: build.stage.$(stage)

stage1 ::
	$(MAKE) stage=1

stage2 ::
	$(MAKE) stage=2

stage3 ::
	$(MAKE) stage=3

clean distclean::
	-$(CABAL) clean --distpref dist-stage1
	-$(CABAL) clean --distpref dist-stage2
	$(RM) -rf stage1-inplace
	$(RM) -rf stage2-inplace

# XXX This should be defined somewhere more central
INPLACE_DATA_DIR = $(FPTOOLS_TOP_ABS)/inplace-datadir

CONFIGURE_FLAGS_STAGE1 += --flags=-ghci

ifeq "$(GhcWithInterpreter)" "YES"
CONFIGURE_FLAGS_STAGE2 += --flags=ghci
else
CONFIGURE_FLAGS_STAGE2 += --flags=-ghci
endif

ifeq "$(GhcProfiled)" "YES"
CONFIGURE_FLAGS_STAGE2 += --enable-executable-profiling
endif
ifeq "$(GhcDebugged)" "YES"
CONFIGURE_FLAGS_STAGE2 += --ghc-option=-debug
endif
ifeq "$(GhcThreaded)" "YES"
# Use threaded RTS with GHCi, so threads don't get blocked at the prompt.
CONFIGURE_FLAGS_STAGE2 += --ghc-option=-threaded
endif

CONFIGURE_FLAGS_STAGE3 = $(CONFIGURE_FLAGS_STAGE2)

CONFIGURE_FLAGS_STAGE1 += $(USE_BOOT_CONFIGURE_FLAGS)
CONFIGURE_FLAGS_STAGE2 += $(USE_STAGE1_CONFIGURE_FLAGS)
CONFIGURE_FLAGS_STAGE3 += $(USE_STAGE2_CONFIGURE_FLAGS)

# XXX In stage2+ we should really use the inplace ghc-pkg
# It works because installPackage doesn't actually use ghc-pkg, as there's
# no library to register

boot.stage.%:
	$(CABAL) configure --distpref dist-stage$* \
	                   $(INSTALL_DIRS_CONFIGURE_FLAGS) \
	                   $(CONFIGURE_FLAGS_STAGE$*) \
	                   $(COMMON_CONFIGURE_FLAGS) \
	                   --datasubdir=. \

build.stage.%:
	$(CABAL) build     --distpref dist-stage$* $(BUILD_FLAGS)
	$(INSTALL_PACKAGE) install '$(GHC_PKG_PROG)' 'XXX/package.conf' "" \
	                   $(FPTOOLS_TOP_ABS)/ghc/stage$*-inplace \
	                   $(FPTOOLS_TOP_ABS)/ghc/stage$*-inplace \
	                   '$$prefix/bin' \
	                   '$$prefix/lib' \
	                   '$$prefix/libexec' \
	                   '$$prefix/dynlib' \
	                   '$(INPLACE_DATA_DIR)' \
	                   '$$prefix/doc' \
	                   '$$prefix/html' \
	                   '$$prefix/haddock' \
	                   --distpref dist-stage$* \
	                   $(INSTALL_FLAGS)

# XXX For now we always install the stage 2 compiler
install:
	$(INSTALL_PACKAGE) install UNUSED UNUSED '$(DESTDIR)' '$(prefix)' \
	                   '$(prefix)' '$(bindir)' '$(libdir)'            \
                       '$(libexecdir)' '$(dynlibdir)' '$(datadir)'    \
                       '$(docdir)' '$(htmldir)' '$(haddockdir)'       \
	                   --distpref dist-stage2                         \
	                   $(INSTALL_FLAGS)

# XXX fix:
#binary-dist:
#	$(INSTALL_DIR)                      $(BIN_DIST_DIR)/utils/hsc2hs
#	$(INSTALL_DATA)    Makefile         $(BIN_DIST_DIR)/utils/hsc2hs/
#	$(INSTALL_DATA)    hsc2hs.sh        $(BIN_DIST_DIR)/utils/hsc2hs/
#	$(INSTALL_DATA)    $(INSTALL_DATAS) $(BIN_DIST_DIR)/utils/hsc2hs/
#	$(INSTALL_PROGRAM) $(HS_PROG)       $(BIN_DIST_DIR)/utils/hsc2hs/

