
TOP=../..
ENABLE_SHELL_WRAPPERS = YES
EXTRA_INPLACE_CONFIGURE_FLAGS += --flags=-ghc-paths
EXTRA_STAGE2_CONFIGURE_FLAGS += --flags=-ghc-paths

include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal.mk
SRC_HC_OPTS += -w

.PHONY: install-inplace

INPLACE_PKG_CONF = $(FPTOOLS_TOP_ABS)/ghc/inplace-datadir/package.conf

install-inplace:
	$(INSTALL_PACKAGE) install '$(GHC_PKG_PROG)' '$(INPLACE_PKG_CONF)' ''  \
	    '$(FPTOOLS_TOP_ABS)/utils/haddock/install-inplace' \
	    '$(FPTOOLS_TOP_ABS)/utils/haddock/install-inplace' \
	    '$$prefix/bin'                                     \
	    '$$prefix/lib'                                     \
	    '$$prefix/libexec'                                 \
	    '$$prefix/dynlib'                                  \
	    '$$prefix/share'                                   \
	    '$$prefix/doc'                                     \
	    '$$prefix/html'                                    \
	    '$$prefix/haddock'                                 \
	    --distpref dist-install --enable-shell-wrappers

