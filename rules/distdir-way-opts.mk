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
 $$($1_$2_HC_OPTS) \
 $$(CONF_HC_OPTS_STAGE$4) \
 $$($1_$2_MORE_HC_OPTS) \
 $$($1_$2_EXTRA_HC_OPTS) \
 $$($1_$2_$3_HC_OPTS) \
 $$($$(basename $$(subst ./,,$$<))_HC_OPTS) \
 $$(SRC_HC_WARNING_OPTS) \
 $$(EXTRA_HC_OPTS)

$1_$2_$3_MOST_DIR_HC_OPTS = \
 $$($1_$2_$3_MOST_HC_OPTS) \
 -odir $1/$2/build -hidir $1/$2/build -stubdir $1/$2/build

# NB. CONF_HC_OPTS_STAGE$4 has to be late enough to override $1_$2_HC_OPTS, so
# that -O0 is effective (see #5484)

# $1_$2_$3_ALL_HC_OPTS: this is all the options we will pass to GHC
# for a given ($1,$2,$3).
$1_$2_$3_ALL_HC_OPTS = \
 -hisuf $$($3_hisuf) -osuf  $$($3_osuf) -hcsuf $$($3_hcsuf) \
 $$($1_$2_$3_MOST_DIR_HC_OPTS) \
 $$(if $$(findstring YES,$$($1_$2_SplitObjs)),$$(if $$(findstring dyn,$3),,-split-objs),) \
 $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),$$(if $$(findstring v,$3),-dynamic-too))

ifeq "$3" "dyn"
ifneq "$4" "0"
ifeq "$$(TargetOS_CPP)" "linux"
$1_$2_$3_GHC_LD_OPTS += \
    -fno-use-rpaths \
    $$(foreach d,$$($1_$2_TRANSITIVE_DEPS),-optl-Wl$$(comma)-rpath -optl-Wl$$(comma)'$$$$ORIGIN/../$$d')
else ifeq "$$(TargetOS_CPP)" "darwin"
$1_$2_$3_GHC_LD_OPTS += -optl-Wl,-headerpad_max_install_names
endif
endif
endif

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
 $$(SRC_AS_OPTS) \
 $$(WAY_$3_AS_OPTS) \
 $$($1_AS_OPTS) \
 $$($1_$2_AS_OPTS) \
 $$($1_$2_$3_AS_OPTS) \
 $$(EXTRA_AS_OPTS)

endef

