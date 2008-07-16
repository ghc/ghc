
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

boot:
	@:

all:: build.stage$(stage)

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

build.stage1:
	$(CABAL) configure --distpref dist-stage1 \
	                   --flags=-ghci \
	                   $(INSTALL_DIRS_CONFIGURE_FLAGS) \
	                   $(USE_BOOT_CONFIGURE_FLAGS) \
	                   $(COMMON_CONFIGURE_FLAGS)
	$(CABAL) build     --distpref dist-stage1 $(BUILD_FLAGS)
	$(INSTALL_PACKAGE) install '$(GHC_PKG_PROG)' 'XXX/package.conf' "" \
	                   $(FPTOOLS_TOP_ABS)/ghc/stage1-inplace \
	                   $(FPTOOLS_TOP_ABS)/ghc/stage1-inplace \
	                   '$$prefix/bin' \
	                   '$$prefix/lib' \
	                   '$$prefix/libexec' \
	                   '$$prefix/dynlib' \
	                   '$$prefix/data' \
	                   '$$prefix/doc' \
	                   '$$prefix/html' \
	                   '$$prefix/haddock' \
	                   --distpref dist-stage1 \
	                   $(INSTALL_FLAGS)

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

# XXX In stage2 we should really use the inplace ghc-pkg
# It works because installPackage doesn't actually use ghc-pkg, as there's
# no library to register

build.stage2:
	$(CABAL) configure --distpref dist-stage2 \
	                   $(CONFIGURE_FLAGS_STAGE2) \
	                   $(INSTALL_DIRS_CONFIGURE_FLAGS) \
	                   $(USE_STAGE1_CONFIGURE_FLAGS) \
	                   $(COMMON_CONFIGURE_FLAGS)
	$(CABAL) build     --distpref dist-stage2 $(BUILD_FLAGS)
	$(INSTALL_PACKAGE) install '$(GHC_PKG_PROG)' 'XXX/package.conf' "" \
	                   $(FPTOOLS_TOP_ABS)/ghc/stage2-inplace \
	                   $(FPTOOLS_TOP_ABS)/ghc/stage2-inplace \
	                   '$$prefix/bin' \
	                   '$$prefix/lib' \
	                   '$$prefix/libexec' \
	                   '$$prefix/dynlib' \
	                   '$$prefix/data' \
	                   '$$prefix/doc' \
	                   '$$prefix/html' \
	                   '$$prefix/haddock' \
	                   --distpref dist-stage2 \
	                   $(INSTALL_FLAGS)

# XXX fix:
#binary-dist:
#	$(INSTALL_DIR)                      $(BIN_DIST_DIR)/utils/hsc2hs
#	$(INSTALL_DATA)    Makefile         $(BIN_DIST_DIR)/utils/hsc2hs/
#	$(INSTALL_DATA)    hsc2hs.sh        $(BIN_DIST_DIR)/utils/hsc2hs/
#	$(INSTALL_DATA)    $(INSTALL_DATAS) $(BIN_DIST_DIR)/utils/hsc2hs/
#	$(INSTALL_PROGRAM) $(HS_PROG)       $(BIN_DIST_DIR)/utils/hsc2hs/

