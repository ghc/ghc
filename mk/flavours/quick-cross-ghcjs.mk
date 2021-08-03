SRC_HC_OPTS        = -O0 -H64m
GhcStage1HcOpts    = -O -ddump-cmm-verbose -ddump-to-file
GhcStage2HcOpts    = -O0
GhcLibHcOpts       = -O
BUILD_PROF_LIBS    = NO
SplitSections      = NO
HADDOCK_DOCS       = NO
BUILD_SPHINX_HTML  = NO
BUILD_SPHINX_PDF   = NO
BUILD_MAN          = NO
WITH_TERMINFO      = NO

INTEGER_LIBRARY      = integer-gmp
Stage1Only           = YES
DYNAMIC_BY_DEFAULT   = NO
DYNAMIC_GHC_PROGRAMS = NO
