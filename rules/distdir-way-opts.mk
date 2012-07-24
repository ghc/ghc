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


# Set compilation flags that additionally depend on a particular way

define distdir-way-opts # args: $1 = dir, $2 = distdir, $3 = way, $4 = stage

# Here is how the command line for a Haskell compilation is constructed.
#   $1 is the directory we're building in
#   $2 is the distdir (e.g. "dist", "dist-install" etc.)
#   $3 is the way (e.g. "v", "p", etc.)
#   $4 is the stage ("1", "2", "3")
# 
# -----------------------------
# The variables affecting Haskell compilations are as follows, including
# the places in the build system that may define them.
#
#  Variable              Purpose                           Defined by
#  --------------        ------------------------------    --------------
#  $1_PACKAGE            Package name for this dir,        $1/$2/ghc.mk
#                        if it is a package   
#   
#  CONF_HC_OPTS          GHC options from ./configure      mk/config.mk.in
#   
#  CONF_HC_OPTS_STAGE$4  GHC options from ./configure      mk/config.mk.in
#                        specific to stage $4   
#   
#  WAY_$3_HC_OPTS        GHC options specific to way $3    mk/ways.mk
#   
#  SRC_HC_OPTS           source-tree-wide GHC options      mk/config.mk.in
#                                                          mk/build.mk
#                                                          mk/validate.mk
#   
#  SRC_HC_WARNING_OPTS   source-tree-wide GHC warning      mk/config.mk.in
#                        options                           mk/build.mk
#                                                          mk/validate.mk
#   
#  EXTRA_HC_OPTS         for supplying extra options on    make EXTRA_HC_OPTS=...
#                        the command line   
#   
#  $1_HC_OPTS            GHC options specific to this      $1/$2/package-data.mk
#                        dir
#   
#  $1_$2_HC_OPTS         GHC options specific to this      $1/$2/package-data.mk
#                        dir and distdir
#   
#  $1_$2_$3_HC_OPTS      GHC options specific to this      $1/$2/package-data.mk
#                        dir, distdir and way
#   
#  $1_$2_MORE_HC_OPTS    GHC options for this dir/distdir  ???
#   
#  $1_$2_EXTRA_HC_OPTS   GHC options for this dir/distdir  mk/build.mk
#   
#  $1_$2_HC_PKGCONF      -package-db flag if necessary   rules/package-config.mk
#   
#  $1_$2_HS_SRC_DIRS     dirs relative to $1 containing    $1/$2/package-data.mk
#                        source files   
#   
#  $1_$2_CPP_OPTS        CPP options                       $1/$2/package-data.mk
#  
#  <file>_HC_OPTS        GHC options for this source       $1/$2/ghc.mk
#                        file (without the extension)

# -----------------------------

# The actual options passed to a Haskell compilation are defined
# below.  Note that in general, more specific sets of options come
# after the less specific, so that we can override global options
# on a per-directory or per-way basis, for example.

# $1_$2_$3_MOST_HC_OPTS is also passed to C compilations when we use
# GHC as the C compiler.

$1_$2_$3_MOST_HC_OPTS = \
 $$(WAY_$3_HC_OPTS) \
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
 $$(if $$(findstring YES,$$($1_$2_SplitObjs)),$$(if $$(findstring dyn,$3),,-split-objs),) \
 $$($1_$2_HC_OPTS) \
 $$(CONF_HC_OPTS_STAGE$4) \
 $$($1_$2_MORE_HC_OPTS) \
 $$($1_$2_EXTRA_HC_OPTS) \
 $$($1_$2_$3_HC_OPTS) \
 $$($$(basename $$(subst ./,,$$<))_HC_OPTS) \
 $$(SRC_HC_WARNING_OPTS) \
 $$(EXTRA_HC_OPTS)

# NB. CONF_HC_OPTS_STAGE$4 has to be late enough to override $1_$2_HC_OPTS, so
# that -O0 is effective (see #5484)

# $1_$2_$3_ALL_HC_OPTS: this is all the options we will pass to GHC
# for a given ($1,$2,$3).
$1_$2_$3_ALL_HC_OPTS = \
 $$($1_$2_$3_MOST_HC_OPTS) \
 -odir $1/$2/build -hidir $1/$2/build -stubdir $1/$2/build \
 -hisuf $$($3_hisuf) -osuf  $$($3_osuf) -hcsuf $$($3_hcsuf)

ifeq "$4" "0"
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

ifneq ($$(strip $$($1_$2_DEP_INCLUDE_DIRS_SINGLE_QUOTED)),)
$1_$2_CC_INC_FLAGS := $$(subst $$(space)',$$(space)$$($1_$2_DEP_INCLUDE_DIRS_FLAG)',$$(space)$$($1_$2_DEP_INCLUDE_DIRS_SINGLE_QUOTED))
endif

# The CONF_CC_OPTS_STAGE$4 options are what we use to get gcc to
# behave correctly, but they are specific to the gcc that we are using.
# If GHC is compiling C code then it will take care of that for us,
# and in the case of the stage 0 compiler it may be using a different
# gcc, so we don't want to use our gcc-specific options.
$1_$2_DIST_GCC_CC_OPTS = \
 $$(CONF_CC_OPTS_STAGE$4) \
 $$($1_$2_DIST_CC_OPTS)

$1_$2_DIST_CC_OPTS = \
 $$(SRC_CC_OPTS) \
 $$($1_CC_OPTS) \
 $$(foreach dir,$$(filter-out /%,$$($1_$2_INCLUDE_DIRS)),-I$1/$$(dir)) \
 $$(foreach dir,$$(filter /%,$$($1_$2_INCLUDE_DIRS)),-I$$(dir)) \
 $$($1_$2_CC_OPTS) \
 $$($1_$2_CPP_OPTS) \
 $$($1_$2_CC_INC_FLAGS) \
 $$($1_$2_DEP_CC_OPTS) \
 $$(SRC_CC_WARNING_OPTS)

ifneq ($$(strip $$($1_$2_DEP_LIB_DIRS_SINGLE_QUOTED)),)
$1_$2_DIST_LD_LIB_DIRS := $$(subst $$(space)',$$(space)-L',$$(space)$$($1_$2_DEP_LIB_DIRS_SINGLE_QUOTED))
endif

$1_$2_DIST_LD_OPTS = \
 $$(CONF_GCC_LINKER_OPTS_STAGE$4) \
 $$(SRC_LD_OPTS) \
 $$($1_LD_OPTS) \
 $$($1_$2_LD_OPTS) \
 $$($1_$2_DIST_LD_LIB_DIRS) \
 $$(foreach opt,$$($1_$2_DEP_EXTRA_LIBS),-l$$(opt)) \
 $$($1_$2_DEP_LD_OPTS)

# c.f. Cabal's Distribution.Simple.PreProcess.ppHsc2hs
# We use '' around cflags and lflags to handle paths with backslashes in
# on Windows
ifneq ($$(strip $$($1_$2_DIST_GCC_CC_OPTS)),)
$1_$2_$3_HSC2HS_CC_OPTS:=$$(shell for i in $$($1_$2_DIST_GCC_CC_OPTS); do echo \'--cflag=$$$$i\'; done)
endif
ifneq ($$(strip $$($1_$2_DIST_LD_OPTS)),)
$1_$2_$3_HSC2HS_LD_OPTS:=$$(shell for i in $$($1_$2_DIST_LD_OPTS); do echo \'--lflag=$$$$i\'; done)
endif

$1_$2_$3_ALL_HSC2HS_OPTS = \
 --cc=$$(WhatGccIsCalled) \
 --ld=$$(WhatGccIsCalled) \
 $$(CONF_HSC2HS_OPTS) \
 $$(SRC_HSC2HS_OPTS) \
 $$(WAY_$3_HSC2HS_OPTS) \
 --cflag=-D__GLASGOW_HASKELL__=$$(if $$(filter 0,$4),$$(GhcCanonVersion),$$(ProjectVersionInt)) \
 --cflag=-D$$(HostArch_CPP)_HOST_ARCH=1 \
 --cflag=-D$$(HostOS_CPP)_HOST_OS=1 \
 $$($1_$2_$3_HSC2HS_CC_OPTS) \
 $$($1_$2_$3_HSC2HS_LD_OPTS) \
 --cflag=-I$1/$2/build/autogen \
 $$(if $$($1_PACKAGE),--cflag=-include --cflag=$1/$2/build/autogen/cabal_macros.h) \
 $$($$(basename $$<)_HSC2HS_OPTS) \
 $$(EXTRA_HSC2HS_OPTS)

$1_$2_$3_ALL_CC_OPTS = \
 $$(WAY_$3_CC_OPTS) \
 $$($1_$2_DIST_GCC_CC_OPTS) \
 $$($1_$2_$3_CC_OPTS) \
 $$($$(basename $$<)_CC_OPTS) \
 $$(EXTRA_CC_OPTS)

$1_$2_$3_GHC_CC_OPTS = \
 $$(addprefix -optc, \
     $$(WAY_$3_CC_OPTS) \
     $$($1_$2_DIST_CC_OPTS) \
     $$($1_$2_$3_CC_OPTS) \
     $$($$(basename $$<)_CC_OPTS) \
     $$(EXTRA_CC_OPTS)) \
 $$($1_$2_$3_MOST_HC_OPTS)

$1_$2_$3_ALL_AS_OPTS = \
 $$(CONF_AS_OPTS) \
 $$(SRC_AS_OPTS)
 $$(WAY_$3_AS_OPTS) \
 $$($1_AS_OPTS) \
 $$($1_$2_AS_OPTS) \
 $$($1_$2_$3_AS_OPTS) \
 $$(EXTRA_AS_OPTS)

$1_$2_$3_ALL_ALEX_OPTS = \
 $$(CONF_ALEX_OPTS) \
 $$(SRC_ALEX_OPTS)
 $$(WAY_$3_ALEX_OPTS) \
 $$($1_ALEX_OPTS) \
 $$($1_$2_ALEX_OPTS) \
 $$($1_$2_$3_ALEX_OPTS) \
 $$(EXTRA_ALEX_OPTS)

$1_$2_$3_ALL_HAPPY_OPTS = \
 $$(CONF_HAPPY_OPTS) \
 $$(SRC_HAPPY_OPTS) \
 $$(WAY_$3_HAPPY_OPTS) \
 $$($1_HAPPY_OPTS) \
 $$($1_$2_HAPPY_OPTS) \
 $$($1_$2_$3_HAPPY_OPTS) \
 $$(EXTRA_HAPPY_OPTS)

endef

