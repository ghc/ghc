
WERROR          = -Werror

HADDOCK_DOCS    = YES
SRC_CC_OPTS     = $(WERROR)
SRC_HC_OPTS     = $(WERROR) -H64m -O0 -fasm
GhcStage1HcOpts = -O -fasm
GhcStage2HcOpts = -O0 -fasm
GhcLibHcOpts    = -O -fasm -dcore-lint
GhcLibWays      =
SplitObjs       = NO
NoFibWays       =
STRIP           = :
GhcBootLibs     = YES

