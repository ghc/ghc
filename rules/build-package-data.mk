define build-package-data # args: $1 = dir, $2 = distdir

ifeq "$(HSCOLOUR_SRCS)" "YES"
$1_$2_CONFIGURE_FLAGS += --with-hscolour="$$(HSCOLOUR)"
endif

# This rule configures the package, generates the package-data.mk file
# for our build system, and registers the package for use in-place in
# the build tree.
$1/$2/package-data.mk $1/$2/inplace-pkg-config $1/$2/build/autogen/cabal_macros.h : $$(GHC_CABAL_INPLACE) $$($1_$2_GHC_PKG_DEP) $1/$$($1_PACKAGE).cabal $$(wildcard $1/configure) $$($1_$2_HC_CONFIG_DEP)
	$$(GHC_CABAL_INPLACE) configure --with-ghc=$$($1_$2_HC_CONFIG) --with-ghc-pkg=$$($1_$2_GHC_PKG) --with-gcc=$$(WhatGccIsCalled) --configure-option=--with-cc=$$(WhatGccIsCalled) $$($1_$2_CONFIGURE_FLAGS) $$($1_CONFIGURE_OPTS) $$($1_$2_CONFIGURE_OPTS) -- $2 $1
	if [ "$$($1_$2_PROG)" = "" ]; then \
	    $$($1_$2_GHC_PKG) update --force $$($1_$2_GHC_PKG_OPTS) $1/$2/inplace-pkg-config; \
	fi

endef
