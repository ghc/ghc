# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------


# Set compilation flags that do not depend on a particular way

define distdir-opts # args: $1 = dir, $2 = distdir, $3 = stage

ifeq "$3" ""
$$(error Stage not given for distdir-opts $1 $2)
endif

ifneq ($$(strip $$($1_$2_DEP_INCLUDE_DIRS_SINGLE_QUOTED)),)
$1_$2_CC_INC_FLAGS := $$(subst $$(space)',$$(space)-I',$$(space)$$($1_$2_DEP_INCLUDE_DIRS_SINGLE_QUOTED))
endif

# The CONF_CC_OPTS_STAGE$3 options are what we use to get gcc to
# behave correctly, but they are specific to the gcc that we are using.
# If GHC is compiling C code then it will take care of that for us,
# and in the case of the stage 0 compiler it may be using a different
# gcc, so we don't want to use our gcc-specific options.
$1_$2_DIST_GCC_CC_OPTS = \
 $$(CONF_CC_OPTS_STAGE$3) \
 $$($1_$2_DIST_CC_OPTS)

$1_$2_DIST_CC_OPTS = \
 $$(SRC_CC_OPTS) \
 $$($1_CC_OPTS) \
 -I$1/$2/build/$$(or $$($1_EXECUTABLE),$$($1_$2_PROGNAME),.)/autogen \
 $$(foreach dir,$$(filter-out /%,$$($1_$2_INCLUDE_DIRS)),-I$1/$$(dir)) \
 $$(foreach dir,$$(filter /%,$$($1_$2_INCLUDE_DIRS)),-I$$(dir)) \
 $$($1_$2_CC_OPTS) \
 $$($1_$2_CPP_OPTS) \
 $$($1_$2_CC_INC_FLAGS) \
 $$($1_$2_DEP_CC_OPTS) \
 $$(SRC_CC_WARNING_OPTS)

$1_$2_DIST_LD_OPTS = \
 $$(SRC_LD_OPTS) \
 $$($1_LD_OPTS) \
 $$($1_$2_LD_OPTS) \
 $$($1_$2_DIST_LD_LIB_DIRS) \
 $$($1_$2_DEP_LD_OPTS)

# c.f. Cabal's Distribution.Simple.PreProcess.ppHsc2hs
# We use '' around cflags and lflags to handle paths with backslashes in
# on Windows
ifneq ($$(strip $$($1_$2_DIST_GCC_CC_OPTS)),)
$1_$2_HSC2HS_CC_OPTS:=$$(shell for i in $$($1_$2_DIST_GCC_CC_OPTS); do echo \'--cflag=$$$$i\'; done)
endif
ifneq ($$(strip $$($1_$2_DIST_LD_OPTS)),)
$1_$2_HSC2HS_LD_OPTS:=$$(shell for i in $$($1_$2_DIST_LD_OPTS); do echo \'--lflag=$$$$i\'; done)
endif

$1_$2_ALL_HSC2HS_OPTS = \
 '--cc=$$(CC_STAGE$3)' \
 '--ld=$$(CC_STAGE$3)' \
 $$(CONF_HSC2HS_OPTS) \
 $$(SRC_HSC2HS_OPTS) \
 $$(SRC_HSC2HS_OPTS_STAGE$3) \
 --cflag=-D__GLASGOW_HASKELL__=$$(if $$(filter 0,$3),$$(GhcCanonVersion),$$(ProjectVersionInt)) \
 $$($1_$2_HSC2HS_CC_OPTS) \
 $$($1_$2_HSC2HS_LD_OPTS) \
 --cflag=-I$1/$2/build/$$(or $$($1_EXECUTABLE),$$($1_$2_PROGNAME),.)/autogen \
 $$(if $$($1_PACKAGE),--cflag=-include --cflag=$1/$2/build/$$(or $$($1_EXECUTABLE),$$($1_$2_PROGNAME),.)/autogen/cabal_macros.h) \
 $$($$(basename $$<)_HSC2HS_OPTS) \
 $$(EXTRA_HSC2HS_OPTS)

$1_$2_ALL_ALEX_OPTS = \
 $$(CONF_ALEX_OPTS) \
 $$(SRC_ALEX_OPTS) \
 $$($1_ALEX_OPTS) \
 $$($1_$2_ALEX_OPTS) \
 $$(EXTRA_ALEX_OPTS)

$1_$2_ALL_HAPPY_OPTS = \
 $$(CONF_HAPPY_OPTS) \
 $$(SRC_HAPPY_OPTS) \
 $$($1_HAPPY_OPTS) \
 $$($1_$2_HAPPY_OPTS) \
 $$(EXTRA_HAPPY_OPTS)

# Disable split sections when building with stage0, it won't be supported yet
# and it's probably not very relevant anyway (smaller stage1 ghc?).
ifeq "$$($1_$2_SplitSections)" ""
ifeq "$3" "1"
$1_$2_SplitSections = $(SplitSections)
else
$1_$2_SplitSections = NO
endif
endif

endef

