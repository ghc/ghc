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


# Build a package with the stage-1 compiler, multiple ways.  A typical
# libraries/foo/ghc.mk will look like this:
#
# $(eval $(call build-package,libraries/base,dist-install))
#
# The package metadata is generated from the .cabal file and placed in
# package-data.mk.  It will look something like this:
#
# libraries/base_dist_MODULES = GHC.Base Data.Tuple ...
# libraries/base_dist_PACKAGE = base
# libraries/base_dist_VERSION = 4.0.0.0
# libraries/base_dist_HC_OPTS = -package ghc-prim-0.1.0.0 -XRank2Types ...
# libraries/base_dist_C_SRCS  = cbits/PrelIOUtils.c ...
# libraries/base_dist_S_SRCS  = cbits/foo.S ...
# libraries/base_dist_CC_OPTS = -Iinclude ...
# libraries/base_dist_LD_OPTS = -package ghc-prim-0.1.0.0

define build-package
$(call trace, build-package($1,$2,$3))
$(call profStart, build-package($1,$2,$3))
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

ifeq "$$(findstring $3,0 1 2)" ""
$$(error $1/$2: stage argument to build-package should be 0, 1, or 2)
endif

$(call clean-target,$1,$2,$1/$2)

distclean : clean_$1_$2_config

.PHONY: clean_$1_$2_config
clean_$1_$2_config:
	$$(call removeFiles,$1/config.log $1/config.status $(wildcard $1/include/Hs*Config.h))
	$$(call removeTrees,$1/autom4te.cache)

ifneq "$$($1_$2_NOT_NEEDED)" "YES"
$$(eval $$(call build-package-helper,$1,$2,$3))
endif
$(call profEnd, build-package($1,$2,$3))
endef


define build-package-helper
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

# --- CONFIGURATION

$(call package-config,$1,$2,$3)

ifeq "$3" "1"
$$($1_PACKAGE)_INSTALL_INFO = $1_$2
endif

# Bootstrapping libs are only built one way
ifeq "$3" "0"
$1_$2_WAYS = v
else
$1_$2_WAYS = $$(filter-out $$($1_$2_EXCLUDED_WAYS),$$(GhcLibWays))
endif

# We must use a different dependency file if $(GhcLibWays) changes, so
# encode the ways into the name of the file.
$1_$2_WAYS_DASHED = $$(subst $$(space),,$$(patsubst %,-%,$$(strip $$($1_$2_WAYS))))
$1_$2_depfile_base = $1/$2/build/.depend$$($1_$2_WAYS_DASHED)

$(call build-package-data,$1,$2,$3)
ifneq "$$(NO_INCLUDE_PKGDATA)" "YES"
ifeq "$3" "0"
include $1/$2/package-data.mk
else ifeq "$(phase)" "final"
include $1/$2/package-data.mk
endif
# Each Haskell compilation in this package will depend on the
# package-data.mk file because e.g. if the version of the package
# changes we need to recompile everything in it.
$1_$2_PKGDATA_DEP = $1/$2/package-data.mk
endif

# We don't bother splitting the bootstrap packages (built with stage 0)
ifeq "$$($1_$2_SplitObjs)" ""
ifeq "$$(SplitObjs) $3" "YES 1"
$1_$2_SplitObjs = YES
else
$1_$2_SplitObjs = NO
endif
endif

$(call hs-sources,$1,$2)
$(call c-sources,$1,$2)
$(call includes-sources,$1,$2)

$(call dependencies,$1,$2,$3)

# Now generate all the build rules for each way in this directory:
$$(foreach way,$$($1_$2_WAYS),$$(eval \
    $$(call c-objs,$1,$2,$$(way)) \
    $$(call c-suffix-rules,$1,$2,$$(way),YES) \
    $$(call cmm-objs,$1,$2,$$(way)) \
    $$(call cmm-suffix-rules,$1,$2,$$(way)) \
    $$(call build-package-way,$1,$2,$$(way),$3) \
  ))

# Programs will need to depend on either the vanilla lib (if -static
# is the default) or the dyn lib (if -dynamic is the default). We
# conservatively make them depend on both, to keep things simple.
# If dyn libs are not being built then $$($1_$2_dyn_LIB) will just
# expand to the empty string, and be ignored.
$1_$2_PROGRAM_DEP_LIB = $$($1_$2_v_LIB) $$($1_$2_dyn_LIB)

# C and S files are possibly built the "dyn" way.
ifeq "$$(BuildSharedLibs)" "YES"
$(call c-objs,$1,$2,dyn)
$(call c-suffix-rules,$1,$2,dyn,YES)
endif

$(call all-target,$1,all_$1_$2)
# This give us things like
#     all_libraries: all_libraries/base_dist-install
ifneq "$$($1_$2_GROUP)" ""
all_$$($1_$2_GROUP): all_$1_$2
endif

ifneq "$3" "0"
$(call haddock,$1,$2)
endif

# Don't put bootstrapping packages in the bindist
ifneq "$3" "0"
BINDIST_EXTRAS += $1/*.cabal $$(wildcard $1/*.buildinfo) $1/$2/setup-config $1/LICENSE
BINDIST_EXTRAS += $$($1_$2_INSTALL_INCLUDES_SRCS)
endif

endef

