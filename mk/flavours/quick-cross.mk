SRC_HC_OPTS        = -O0 -H64m
GhcStage0HcOpts    = -O
GhcStage1HcOpts    = -O0 -fllvm
GhcLibHcOpts       = -O -fllvm
BUILD_PROF_LIBS    = NO
SplitSections      = NO
HADDOCK_DOCS       = NO
BUILD_SPHINX_HTML  = NO
BUILD_SPHINX_PDF   = NO
BUILD_MAN          = NO
WITH_TERMINFO      = NO

BIGNUM_BACKEND       = native
Stage0Only           = YES
DYNAMIC_GHC_PROGRAMS = NO
