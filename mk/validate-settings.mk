# DO NOT EDIT!  Instead, create a file mk/validate.mk, whose settings will
# override these.  See also mk/custom-settings.mk.

WERROR              = -Werror
SRC_CC_WARNING_OPTS =
SRC_HC_WARNING_OPTS =

HADDOCK_DOCS    = YES

#####################
# Warnings

# Debian doesn't turn -Werror=unused-but-set-variable on by default, so
# we turn it on explicitly for consistency with other users
ifeq "$(GccLT46)" "NO"
SRC_CC_WARNING_OPTS += -Werror=unused-but-set-variable
# gcc 4.6 gives 3 warning for giveCapabilityToTask not being inlined
SRC_CC_WARNING_OPTS += -Wno-error=inline
endif

SRC_CC_OPTS     += $(WERROR) -Wall
SRC_HC_OPTS     += $(WERROR) -Wall

GhcStage1HcOpts += -fwarn-tabs
GhcStage2HcOpts += -fwarn-tabs
utils/hpc_dist-install_EXTRA_HC_OPTS += -fwarn-tabs

#####################
SRC_HC_OPTS     += -H64m -O0

GhcStage1HcOpts += -O
GhcStage2HcOpts += -O -dcore-lint
# Using -O (rather than -O0) here bringes my validate down from 22mins to 16 mins.
# Compiling stage2 takes longer, but we gain a faster haddock, faster
# running of the tests, and faster building of the utils to be installed

GhcLibHcOpts    += -O -dcore-lint

# We define DefaultFastGhcLibWays in this style so that the value is
# correct even if the user alters DYNAMIC_BY_DEFAULT
DefaultFastGhcLibWays = $(if $(filter $(DYNAMIC_BY_DEFAULT),YES),dyn,v)
DefaultProfGhcLibWays = $(if $(filter $(GhcProfiled),YES),p,)

ifeq "$(ValidateSpeed)" "FAST"
GhcLibWays     = $(DefaultFastGhcLibWays)
else
GhcLibWays     := $(filter v dyn,$(GhcLibWays))
endif
GhcLibWays     += $(DefaultProfGhcLibWays)
SplitObjs       = NO
NoFibWays       =
STRIP_CMD       = :

CHECK_PACKAGES = YES

# We want to install DPH when validating, so that we can test it
InstallExtraPackages = YES

# dblatex with miktex under msys/mingw can't build the PS and PDF docs,
# and just building the HTML docs is sufficient to check that the
# markup is correct, so we turn off PS and PDF doc building when
# validating.
BUILD_DOCBOOK_PS  = NO
BUILD_DOCBOOK_PDF = NO

ifeq "$(ValidateHpc)" "YES"
GhcStage2HcOpts += -fhpc -hpcdir $(TOP)/testsuite/hpc_output/
endif
ifeq "$(ValidateSpeed)" "SLOW"
GhcStage2HcOpts += -DDEBUG
endif

######################################################################
# Disable some warnings in packages we use

# Cabal doesn't promise to be warning-free
utils/ghc-cabal_dist_EXTRA_HC_OPTS += -w
libraries/Cabal/Cabal_dist-boot_EXTRA_HC_OPTS += -w
libraries/Cabal/Cabal_dist-install_EXTRA_HC_OPTS += -w

# Temporarily turn off incomplete-pattern warnings for containers
libraries/containers_dist-install_EXTRA_HC_OPTS += -fno-warn-incomplete-patterns

# Temporarily turn off pointless-pragma warnings for containers
libraries/containers_dist-install_EXTRA_HC_OPTS += -fno-warn-pointless-pragmas

# bytestring has identities at the moment
libraries/bytestring_dist-install_EXTRA_HC_OPTS += -fno-warn-identities

# bytestring uses bitSize at the moment
libraries/bytestring_dist-install_EXTRA_HC_OPTS += -fno-warn-deprecations

# containers uses bitSize at the moment
libraries/containers_dist-install_EXTRA_HC_OPTS += -fno-warn-deprecations

# Temporarily turn off unused-do-bind warnings for the time package
libraries/time_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-do-bind 
# Temporary: mkTyCon is deprecated
libraries/time_dist-install_EXTRA_HC_OPTS += -fno-warn-deprecations
# On Windows, there are also some unused import warnings
libraries/time_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports -fno-warn-identities

# haskeline has warnings about deprecated use of block/unblock
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -fno-warn-deprecations
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports

# binary upstream has some warnings, so don't use -Werror for it
libraries/binary_dist-install_EXTRA_HC_OPTS += -Wwarn

# temporarily turn off -Werror for mtl
libraries/mtl_dist-install_EXTRA_HC_OPTS += -Wwarn

# primitive has a warning about deprecated use of GHC.IOBase
libraries/primitive_dist-install_EXTRA_HC_OPTS += -Wwarn

# temporarily turn off -Werror for transformers
libraries/transformers_dist-boot_EXTRA_HC_OPTS += -Wwarn
libraries/transformers_dist-install_EXTRA_HC_OPTS += -Wwarn

# vector has some unused match warnings
libraries/vector_dist-install_EXTRA_HC_OPTS += -Wwarn

libraries/dph/dph-base_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-interface_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-seq_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-par_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-lifted-common-install_EXTRA_HC_OPTS += -Wwarn

# We need to turn of deprecated warnings for SafeHaskell transition
libraries/array_dist-install_EXTRA_HC_OPTS += -fno-warn-warnings-deprecations

# Temporarely disable inline rule shadowing warning
libraries/bytestring_dist-install_EXTRA_HC_OPTS += -fno-warn-inline-rule-shadowing
libraries/template-haskell_dist-install_EXTRA_HC_OPTS += -fno-warn-inline-rule-shadowing

# We need -fno-warn-deprecated-flags to avoid failure with -Werror
GhcLibHcOpts += -fno-warn-deprecated-flags
GhcBootLibHcOpts += -fno-warn-deprecated-flags
