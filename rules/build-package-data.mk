# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

define build-package-data
# args:
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

ifeq "$$(filter p,$$(GhcLibWays))" "p"
$1_$2_CONFIGURE_OPTS += --enable-library-profiling
endif

ifeq "$$(filter dyn,$$(GhcLibWays))" "dyn"
$1_$2_CONFIGURE_OPTS += --enable-shared
endif

ifeq "$$(GhcWithInterpreter) $$(UseArchivesForGhci)" "YES NO"
$1_$2_CONFIGURE_OPTS += --enable-library-for-ghci
else
$1_$2_CONFIGURE_OPTS += --disable-library-for-ghci
endif

ifeq "$$(HSCOLOUR_SRCS)" "YES"
$1_$2_CONFIGURE_OPTS += --with-hscolour="$$(HSCOLOUR_CMD)"
endif

# We filter out -Werror from SRC_CC_OPTS, because when configure tests
# for a feature it may not generate warning-free C code, and thus may
# think that the feature doesn't exist if -Werror is on.
$1_$2_CONFIGURE_OPTS += --configure-option=CFLAGS="$$(filter-out -Werror,$$(SRC_CC_OPTS)) $$(CONF_CC_OPTS_STAGE$3) $$($1_CC_OPTS) $$($1_$2_CC_OPTS)"
$1_$2_CONFIGURE_OPTS += --configure-option=LDFLAGS="$$(SRC_LD_OPTS) $$($1_LD_OPTS) $$($1_$2_LD_OPTS)"

ifneq "$$(ICONV_INCLUDE_DIRS)" ""
$1_$2_CONFIGURE_OPTS += --configure-option=--with-iconv-includes="$$(ICONV_INCLUDE_DIRS)"
endif

ifneq "$$(ICONV_LIB_DIRS)" ""
$1_$2_CONFIGURE_OPTS += --configure-option=--with-iconv-libraries="$$(ICONV_LIB_DIRS)"
endif

ifneq "$$(GMP_INCLUDE_DIRS)" ""
$1_$2_CONFIGURE_OPTS += --configure-option=--with-gmp-includes="$$(GMP_INCLUDE_DIRS)"
endif

ifneq "$$(GMP_LIB_DIRS)" ""
$1_$2_CONFIGURE_OPTS += --configure-option=--with-gmp-libraries="$$(GMP_LIB_DIRS)"
endif

ifeq "$3" "0"
$1_$2_CONFIGURE_OPTS += $$(BOOT_PKG_CONSTRAINTS)
endif

# This rule configures the package, generates the package-data.mk file
# for our build system, and registers the package for use in-place in
# the build tree.
$1/$2/package-data.mk $1/$2/inplace-pkg-config $1/$2/build/autogen/cabal_macros.h : $$(GHC_CABAL_INPLACE) $$($1_$2_GHC_PKG_DEP) $1/$$($1_PACKAGE).cabal $$(wildcard $1/configure) $$($1_$2_HC_CONFIG_DEP)
	"$$(GHC_CABAL_INPLACE)" configure --with-ghc="$$($1_$2_HC_CONFIG)" --with-ghc-pkg="$$($1_$2_GHC_PKG)" --with-gcc="$$(WhatGccIsCalled)" --configure-option=--with-cc="$$(WhatGccIsCalled)" $$($1_CONFIGURE_OPTS) $$($1_$2_CONFIGURE_OPTS) -- $2 $1
ifeq "$$($1_$2_PROG)" ""
ifneq "$$($1_$2_REGISTER_PACKAGE)" "NO"
ifeq "$$(ghc_ge_6102) $3" "NO 0" # NOTE [1] below
	    cat $1/$2/inplace-pkg-config | sed "s@^import-dirs:@import-dirs: $(TOP)/$1 $(TOP)/$1/src @" | "$$($1_$2_GHC_PKG)" update --force $$($1_$2_GHC_PKG_OPTS) -
else
	    "$$($1_$2_GHC_PKG)" update --force $$($1_$2_GHC_PKG_OPTS) $1/$2/inplace-pkg-config
endif
endif
endif

# [1] this is a hack for GHC <= 6.10.1.  When making dependencies with
# ghc -M, in GHC 6.10.1 and earlier, GHC needed to find either the .hi
# file or the source file for any dependency.  Since we build the
# .depend files before building the packages, we have to make sure GHC
# can find the source files; hence we have to make sure that the
# import-dirs field of each boot package points to the sources for the
# package as well as the dist/build dir.
#
# In GHC 6.10.2, we changed the way ghc -M worked so that it doesn't
# check for existence of the source file, and doesn't look for the .hi
# file if there is only one possibility for its location.  Which means
# that we must *not* do that above hack in this case, because there
# would be multiple locations to search for the .hi file.

endef
