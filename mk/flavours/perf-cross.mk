SRC_HC_OPTS        = -O -H64m
GhcStage0HcOpts    = -O2
GhcStage1HcOpts    = -O2 -fllvm
GhcLibHcOpts       = -O2 -fllvm
BUILD_PROF_LIBS    = YES
HADDOCK_DOCS       = NO
BUILD_SPHINX_HTML  = NO
BUILD_SPHINX_PDF   = NO
BUILD_MAN          = NO
WITH_TERMINFO      = NO

BIGNUM_BACKEND       = native
Stage0Only           = YES
DYNAMIC_GHC_PROGRAMS = NO
