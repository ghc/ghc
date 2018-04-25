# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

# Bootstrapping ghc-cabal

# Euch, hideous hack:
# XXX This should be in a different Makefile
CABAL_DOTTED_VERSION := $(shell grep "^version:" libraries/Cabal/Cabal/Cabal.cabal | sed "s/^version: *//")
CABAL_VERSION := $(subst .,$(comma),$(CABAL_DOTTED_VERSION))
CABAL_CONSTRAINT := --constraint="Cabal == $(CABAL_DOTTED_VERSION)"

# Starting with GHC 8.0 we make use of GHC's native ability to
# generate MIN_VERSION_<pkgname>() CPP macros. The generation of those
# macros is triggered by `-hide-all-packages`, so we have to explicitly
# enumerate all packages we need in scope.
ifeq "$(Windows_Host)" "YES"
CABAL_BUILD_DEPS := ghc-prim base array transformers time containers bytestring deepseq process pretty directory Win32
else
CABAL_BUILD_DEPS := ghc-prim base array transformers time containers bytestring deepseq process pretty directory unix
endif

ghc-cabal_DIST_BINARY_NAME = ghc-cabal$(exeext0)
ghc-cabal_DIST_BINARY = utils/ghc-cabal/dist/build/tmp/$(ghc-cabal_DIST_BINARY_NAME)
ghc-cabal_INPLACE = inplace/bin/$(ghc-cabal_DIST_BINARY_NAME)

ifneq "$(BINDIST)" "YES"
$(ghc-cabal_INPLACE) : $(ghc-cabal_DIST_BINARY) | $$(dir $$@)/.
	"$(CP)" $< $@

# Minor hack, since we can't reuse the `hs-suffix-rules-srcdir` macro
ifneq ($(wildcard libraries/Cabal/Cabal/Distribution/Parsec/Lexer.x),)
# Lexer.x exists so we have to call Alex ourselves
CABAL_LEXER_DEP := bootstrapping/Cabal/Distribution/Parsec/Lexer.hs

bootstrapping/Cabal/Distribution/Parsec/Lexer.hs: libraries/Cabal/Cabal/Distribution/Parsec/Lexer.x
	mkdir -p bootstrapping/Cabal/Distribution/Parsec
	$(call cmd,ALEX) $< -o $@
else
CABAL_LEXER_DEP := libraries/Cabal/Cabal/Distribution/Parsec/Lexer.hs
endif

$(ghc-cabal_DIST_BINARY): $(wildcard libraries/Cabal/Cabal/Distribution/*/*/*.hs)
$(ghc-cabal_DIST_BINARY): $(wildcard libraries/Cabal/Cabal/Distribution/*/*.hs)
$(ghc-cabal_DIST_BINARY): $(wildcard libraries/Cabal/Cabal/Distribution/*.hs)

$(ghc-cabal_DIST_BINARY): $(CABAL_LEXER_DEP) utils/ghc-cabal/Main.hs $(TOUCH_DEP) | $$(dir $$@)/. bootstrapping/.
	"$(GHC)" $(SRC_HC_OPTS) \
	       $(addprefix -optc, $(SRC_CC_OPTS) $(CONF_CC_OPTS_STAGE0)) \
	       $(addprefix -optl, $(SRC_LD_OPTS) $(CONF_GCC_LINKER_OPTS_STAGE0)) \
	       -hide-all-packages \
	       $(addprefix -package , $(CABAL_BUILD_DEPS)) \
	       --make utils/ghc-cabal/Main.hs -o $@ \
	       -no-user-$(GHC_PACKAGE_DB_FLAG) \
	       -Wall -fno-warn-unused-imports -fno-warn-warnings-deprecations \
	       -DCABAL_VERSION=$(CABAL_VERSION) \
	       -DCABAL_PARSEC \
	       -DBOOTSTRAPPING \
	       -odir  bootstrapping \
	       -hidir bootstrapping \
	       $(CABAL_LEXER_DEP) \
	       -ilibraries/Cabal/Cabal \
	       -ilibraries/binary/src \
	       -ilibraries/filepath \
	       -ilibraries/hpc \
	       -ilibraries/mtl \
	       -ilibraries/text \
	       libraries/text/cbits/cbits.c \
	       -Ilibraries/text/include \
	       -ilibraries/parsec \
	       $(utils/ghc-cabal_dist_EXTRA_HC_OPTS) \
	       $(EXTRA_HC_OPTS)
	"$(TOUCH_CMD)" $@
endif

# touch is required, because otherwise if mkdirhier is newer, we
# repeatedly rebuild ghc-cabal.

$(eval $(call clean-target,utils/ghc-cabal,dist,\
   utils/ghc-cabal/dist bootstrapping))

$(eval $(call all-target,utils/ghc-cabal,$(ghc-cabal_INPLACE)))

# -----------------------------------------------------------------------------
# Now make another copy that goes in bindists. This needs to be built
# with the in-tree compiler.

utils/ghc-cabal_USES_CABAL                   = YES
utils/ghc-cabal_PACKAGE                      = ghc-cabal
utils/ghc-cabal_dist-install_PROGNAME        = ghc-cabal
utils/ghc-cabal_dist-install_INSTALL_INPLACE = NO
utils/ghc-cabal_dist-install_WANT_BINDIST_WRAPPER = YES
utils/ghc-cabal_dist-install_MODULES         = Main

$(eval $(call build-prog,utils/ghc-cabal,dist-install,1))
