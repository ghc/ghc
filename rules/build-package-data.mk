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
$(call trace, build-package-data($1,$2,$3))
$(call profStart, build-package-data($1,$2,$3))
# args:
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

$1_$2_CONFIGURE_OPTS += --disable-library-for-ghci
ifeq "$$(filter v,$$($1_$2_WAYS))" "v"
$1_$2_CONFIGURE_OPTS += --enable-library-vanilla
ifeq "$$(GhcWithInterpreter)" "YES"
ifneq "$$(DYNAMIC_GHC_PROGRAMS)" "YES"
$1_$2_CONFIGURE_OPTS += --enable-library-for-ghci
endif
endif
else
$1_$2_CONFIGURE_OPTS += --disable-library-vanilla
endif

ifeq "$$(filter p,$$($1_$2_WAYS))" "p"
$1_$2_CONFIGURE_OPTS += --enable-library-profiling
else
$1_$2_CONFIGURE_OPTS += --disable-library-profiling
endif

ifeq "$$(filter dyn,$$($1_$2_WAYS))" "dyn"
$1_$2_CONFIGURE_OPTS += --enable-shared
else
$1_$2_CONFIGURE_OPTS += --disable-shared
endif

ifeq "$$(HSCOLOUR_SRCS)" "YES"
$1_$2_CONFIGURE_OPTS += --with-hscolour="$$(HSCOLOUR_CMD)"
endif

# We filter out -Werror from SRC_CC_OPTS, because when configure tests
# for a feature it may not generate warning-free C code, and thus may
# think that the feature doesn't exist if -Werror is on.
$1_$2_CONFIGURE_OPTS += --configure-option=CFLAGS="$$(filter-out -Werror,$$(SRC_CC_OPTS)) $$(CONF_CC_OPTS_STAGE$3) $$($1_CC_OPTS) $$($1_$2_CC_OPTS) $$(SRC_CC_WARNING_OPTS)"
$1_$2_CONFIGURE_OPTS += --configure-option=LDFLAGS="$$(SRC_LD_OPTS) $$(CONF_GCC_LINKER_OPTS_STAGE$3) $$($1_LD_OPTS) $$($1_$2_LD_OPTS)"
$1_$2_CONFIGURE_OPTS += --configure-option=CPPFLAGS="$$(SRC_CPP_OPTS) $$(CONF_CPP_OPTS_STAGE$3) $$($1_CPP_OPTS) $$($1_$2_CPP_OPTS)"

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

ifeq "$$(CrossCompiling)" "YES"
$1_$2_CONFIGURE_OPTS += --configure-option=--host=$(TargetPlatformFull)
endif

ifeq "$3" "0"
$1_$2_CONFIGURE_OPTS += $$(BOOT_PKG_CONSTRAINTS)
endif

$1_$2_CONFIGURE_OPTS += --with-gcc="$$(CC_STAGE$3)"

ifneq "$3" "0"
# There is no LD_STAGE0, Cabal will figure it out
$1_$2_CONFIGURE_OPTS += --with-ld="$$(LD_STAGE$3)"
endif

$1_$2_CONFIGURE_OPTS += --configure-option=--with-cc="$$(CC_STAGE$3)"
$1_$2_CONFIGURE_OPTS += --with-ar="$$(AR_STAGE$3)"
$1_$2_CONFIGURE_OPTS += --with-ranlib="$$(RANLIB)"
$1_$2_CONFIGURE_OPTS += $$(if $$(ALEX),--with-alex="$$(ALEX)")
$1_$2_CONFIGURE_OPTS += $$(if $$(HAPPY),--with-happy="$$(HAPPY)")

ifneq "$$(BINDIST)" "YES"
ifneq "$$(NO_GENERATED_MAKEFILE_RULES)" "YES"
$1/$2/inplace-pkg-config : $1/$2/package-data.mk
$1/$2/build/autogen/cabal_macros.h : $1/$2/package-data.mk

# This rule configures the package, generates the package-data.mk file
# for our build system, and registers the package for use in-place in
# the build tree.
$1/$2/package-data.mk : $$$$(ghc-cabal_INPLACE) $$($1_$2_GHC_PKG_DEP) $1/$$($1_PACKAGE).cabal $$(wildcard $1/configure) $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_CONFIG_DEP)
# Checking packages built with the bootstrapping compiler would
# generally be a waste of time. Either we will rebuild them with
# stage1/stage2, or we don't really care about them.
ifneq "$3" "0"
ifneq "$$($1_NO_CHECK)" "YES"
	"$$(ghc-cabal_INPLACE)" check $1
endif
endif
	"$$(ghc-cabal_INPLACE)" configure --with-ghc="$$($1_$2_HC_CONFIG)" --with-ghc-pkg="$$($1_$2_GHC_PKG)" $$($1_CONFIGURE_OPTS) $$($1_$2_CONFIGURE_OPTS) -- $2 $1
ifeq "$$($1_$2_PROG)" ""
ifneq "$$($1_$2_REGISTER_PACKAGE)" "NO"
	$$(call cmd,$1_$2_GHC_PKG) update --force $$($1_$2_GHC_PKG_OPTS) $1/$2/inplace-pkg-config
endif
endif
endif
endif

PACKAGE_DATA_MKS += $1/$2/package-data.mk

$(call profEnd, build-package-data($1,$2,$3))
endef
