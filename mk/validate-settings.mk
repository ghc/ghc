# DO NOT EDIT!  Instead, create a file mk/validate.mk, whose settings will
# override these.  See also mk/custom-settings.mk.

WERROR          = -Werror

HADDOCK_DOCS    = YES

SRC_CC_OPTS     += -Wall $(WERROR)
# Debian doesn't turn -Werror=unused-but-set-variable on by default, so
# we turn it on explicitly for consistency with other users
ifeq "$(GccLT46)" "NO"
SRC_CC_OPTS	    += -Werror=unused-but-set-variable
endif
# gcc 4.6 gives 3 warning for giveCapabilityToTask not being inlined
SRC_CC_OPTS     += -Wno-error=inline

SRC_HC_OPTS     += -Wall $(WERROR) -H64m -O0

# Safe by default
#SRC_HC_OPTS += -Dsh_SAFE_DEFAULT

GhcStage1HcOpts += -O

GhcStage2HcOpts += -O
# Using -O (rather than -O0) here bringes my validate down from 22mins to 16 mins.
# Compiling stage2 takes longer, but we gain a faster haddock, faster
# running of the tests, and faster building of the utils to be installed

GhcLibHcOpts    += -O -dcore-lint
GhcLibWays     := $(filter v dyn,$(GhcLibWays))
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
ifeq "$(ValidateSlow)" "YES"
GhcStage2HcOpts += -DDEBUG
endif

######################################################################
# Disable some warnings in packages we use

# Temporarily turn off incomplete-pattern warnings for containers
libraries/containers_dist-install_EXTRA_HC_OPTS += -fno-warn-incomplete-patterns

# bytestring has identities at the moment
libraries/bytestring_dist-install_EXTRA_HC_OPTS += -fno-warn-identities

# Temporarily turn off unused-do-bind warnings for the time package
libraries/time_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-do-bind 
# Temporary: mkTyCon is deprecated
libraries/time_dist-install_EXTRA_HC_OPTS += -fno-warn-deprecations
# On Windows, there are also some unused import warnings
libraries/time_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports -fno-warn-identities

# haskeline has warnings about deprecated use of block/unblock
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -fno-warn-deprecations
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports

# Temporarily turn off unused-import warnings for the binary package
libraries/binary_dist-boot_EXTRA_HC_OPTS += -fno-warn-unused-imports
libraries/binary_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports -fno-warn-identities

# Temporarily turn off -Werror for some Hoopl modules that have
# non-exhaustive pattern-match warnings
libraries/hoopl/src/Compiler/Hoopl/Util_HC_OPTS += -Wwarn
libraries/hoopl/src/Compiler/Hoopl/GraphUtil_HC_OPTS += -Wwarn
libraries/hoopl/src/Compiler/Hoopl/MkGraph_HC_OPTS += -Wwarn
libraries/hoopl/src/Compiler/Hoopl/XUtil_HC_OPTS += -Wwarn
libraries/hoopl/src/Compiler/Hoopl/Pointed_HC_OPTS += -Wwarn
libraries/hoopl/src/Compiler/Hoopl/Passes/Dominator_HC_OPTS += -Wwarn

# primitive has a warning about deprecated use of GHC.IOBase
libraries/primitive_dist-install_EXTRA_HC_OPTS += -Wwarn

# vector has some unused match warnings
libraries/vector_dist-install_EXTRA_HC_OPTS += -Wwarn

libraries/dph/dph-base_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-interface_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-seq_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-par_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-seq_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-par_dist-install_EXTRA_HC_OPTS += -Wwarn

# We need to turn of deprecated warnings for SafeHaskell transition
libraries/array_dist-install_EXTRA_HC_OPTS += -fno-warn-warnings-deprecations
libraries/binary_dist-install_EXTRA_HC_OPTS += -fno-warn-warnings-deprecations

# We need -fno-warn-deprecated-flags to avoid failure with -Werror
GhcLibHcOpts += -fno-warn-deprecated-flags
GhcBootLibHcOpts += -fno-warn-deprecated-flags

