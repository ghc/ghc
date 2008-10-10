
TOP=../..
include $(TOP)/mk/boilerplate.mk

ENABLE_SHELL_WRAPPERS = YES
EXTRA_INPLACE_CONFIGURE_FLAGS += --flags=in-ghc-tree
EXTRA_STAGE2_CONFIGURE_FLAGS += --flags=in-ghc-tree
EXTRA_STAGE2_CONFIGURE_FLAGS += --datasubdir=.
# If we are profiling GHC then we didn't bother to build the GHC API for
# profiling, so we need to use profiling when building haddock
ifeq "$(GhcProfiled)" "YES"
EXTRA_STAGE2_CONFIGURE_FLAGS += --disable-library-for-ghci
EXTRA_STAGE2_CONFIGURE_FLAGS += --disable-library-vanilla
EXTRA_STAGE2_CONFIGURE_FLAGS += --enable-library-profiling
EXTRA_STAGE2_CONFIGURE_FLAGS += --enable-executable-profiling
endif

# Ideally we'd automatically find these from the .cabal file:
BINDIST_EXTRAS += html/haddock-DEBUG.css \
                  html/haddock.css       \
                  html/haddock-util.js   \
                  html/haskell_icon.gif  \
                  html/minus.gif         \
                  html/plus.gif

include $(TOP)/mk/cabal.mk

# bindist.mk isn't expecting a library to be in the same package as an
# executable, so we have to help it out
LIB_DIST_DIR = $(EXE_DIST_DIR)

SRC_HC_OPTS += -w

.PHONY: install-inplace

INPLACE_PKG_CONF = $(FPTOOLS_TOP_ABS)/ghc/inplace-datadir/package.conf

ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALL_INPLACE_FLAGS += --enable-shell-wrappers
endif

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INPLACE_DATADIR = '$$prefix'
else
INPLACE_DATADIR = '$$prefix/share'
endif


ifeq "$(BuildSharedLibs)" "YES"
DYN_FLAGS = --ghc-option=-dynamic
endif

install-inplace:
	$(INSTALL_PACKAGE) install '$(GHC_PKG_PROG)' '$(INPLACE_PKG_CONF)' ''  \
	    '$(FPTOOLS_TOP_ABS)/utils/haddock/install-inplace' \
	    '$(FPTOOLS_TOP_ABS)/utils/haddock/install-inplace' \
	    '$$prefix/bin'                                     \
	    '$$prefix/lib'                                     \
	    '$$prefix/libexec'                                 \
	    '$$prefix/dynlib'                                  \
	    $(INPLACE_DATADIR)                                 \
	    '$$prefix/doc'                                     \
	    '$$prefix/html'                                    \
	    '$$prefix/haddock'                                 \
	    --distpref dist-install                            \
	    $(INSTALL_INPLACE_FLAGS)
ifeq "$(BuildSharedLibs)" "YES"
	    mv dist-install/build/haddock/haddock.dyn dist-install/build/haddock/haddock # remove the wrapper
endif