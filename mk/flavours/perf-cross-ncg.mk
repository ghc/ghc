SRC_HC_OPTS        = -O -H64m
GhcStage1HcOpts    = -O2
GhcStage2HcOpts    = -O2
GhcLibHcOpts       = -O2
BUILD_PROF_LIBS    = YES
#SplitObjs
HADDOCK_DOCS       = NO
BUILD_SPHINX_HTML  = NO
BUILD_SPHINX_PDF   = NO
BUILD_MAN          = NO
WITH_TERMINFO      = NO

INTEGER_LIBRARY      = integer-simple
Stage1Only           = YES
DYNAMIC_BY_DEFAULT   = NO
DYNAMIC_GHC_PROGRAMS = NO
