SRC_HC_OPTS        = -O -H64m
GhcStage1HcOpts    = -O2
GhcStage2HcOpts    = -O2 -fllvm
GhcLibHcOpts       = -O2 -fllvm
BUILD_PROF_LIBS    = YES
#SplitObjs
HADDOCK_DOCS       = NO
BUILD_DOCBOOK_HTML = NO
BUILD_DOCBOOK_PS   = NO
BUILD_DOCBOOK_PDF  = NO

INTEGER_LIBRARY      = integer-simple
Stage1Only           = YES
DYNAMIC_BY_DEFAULT   = NO
DYNAMIC_GHC_PROGRAMS = NO
