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


# Set compilation flags that depend on a particular directory/distdir

define distdir-opts # args: $1 = dir, $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

ifeq "$3" "0"
# This is a bit of a hack.
# If we are compiling something with the bootstrapping compiler on
# cygwin, and it uses an include file from the rts (say), then we
# need to stop mkdependC from generating a dependincy on
#     c:/ghc/rts/include/Rts.h
# as that confuses make. So we use -isystem instead of -I, which stops
# these dependencies from being generated. Technically this is wrong if
# we depend on a library that is built inside the build tree, and we
# use headers from that library, but currently I don't think that's the
# case.
$1_$2_DEP_INCLUDE_DIRS_FLAG = -isystem
else
$1_$2_DEP_INCLUDE_DIRS_FLAG = -I
endif

$1_$2_DIST_CC_OPTS = \
 $$(CONF_CC_OPTS) \
 $$(SRC_CC_OPTS) \
 $$($1_CC_OPTS) \
 $$(foreach dir,$$(filter-out /%,$$($1_$2_INCLUDE_DIRS)),-I$1/$$(dir)) \
 $$(foreach dir,$$(filter /%,$$($1_$2_INCLUDE_DIRS)),-I$$(dir)) \
 $$($1_$2_CC_OPTS) \
 $$($1_$2_CPP_OPTS) \
 $$(foreach dir,$$($1_$2_DEP_INCLUDE_DIRS),$$($1_$2_DEP_INCLUDE_DIRS_FLAG)$$(dir)) \
 $$($1_$2_DEP_CC_OPTS)

$1_$2_DIST_LD_OPTS = \
 $$(CONF_LD_OPTS) \
 $$(SRC_LD_OPTS) \
 $$($1_LD_OPTS) \
 $$($1_$2_LD_OPTS) \
 $$(foreach opt,$$($1_$2_DEP_LIB_DIRS),-L$$(opt)) \
 $$(foreach opt,$$($1_$2_DEP_EXTRA_LIBS),-l$$(opt)) \
 $$($1_$2_DEP_LD_OPTS)

# c.f. Cabal's Distribution.Simple.GHC.ghcOptions
$1_$2_DIST_HC_OPTS = \
 $$(CONF_HC_OPTS) \
 $$(SRC_HC_OPTS) \
 $$($1_HC_OPTS) \
 $$($1_$2_HC_PKGCONF) \
 $$(if $$($1_$2_PROG),, \
        $$(if $$($1_PACKAGE),-package-name $$($1_PACKAGE)-$$($1_$2_VERSION))) \
 $$(if $$($1_PACKAGE),-hide-all-packages) \
 -i $$(if $$($1_$2_HS_SRC_DIRS),$$(foreach dir,$$($1_$2_HS_SRC_DIRS),-i$1/$$(dir)),-i$1) \
 -i$1/$2/build -i$1/$2/build/autogen \
 -I$1/$2/build -I$1/$2/build/autogen \
 $$(foreach dir,$$(filter-out /%,$$($1_$2_INCLUDE_DIRS)),-I$1/$$(dir)) \
 $$(foreach dir,$$(filter /%,$$($1_$2_INCLUDE_DIRS)),-I$$(dir)) \
 $$(foreach inc,$$($1_$2_INCLUDE),-\#include "$$(inc)") \
 $$(foreach opt,$$($1_$2_CPP_OPTS),-optP$$(opt)) \
 $$(if $$($1_PACKAGE),-optP-include -optP$1/$2/build/autogen/cabal_macros.h) \
 $$(foreach pkg,$$($1_$2_DEPS),-package $$(pkg)) \
 $$(if $$(findstring YES,$$($1_$2_SplitObjs)),-split-objs,) \
 $$($1_$2_HC_OPTS) \
 $$($1_$2_EXTRA_HC_OPTS)

endef

