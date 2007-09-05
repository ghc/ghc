
HADDOCK_DOCS    = YES
SRC_CC_OPTS     = -Werror
SRC_HC_OPTS     = -Werror -H64m -Onot -fasm
GhcStage1HcOpts = -O -fasm
GhcStage2HcOpts = -Onot -fasm
GhcLibHcOpts    = -O -fasm
GhcLibWays      =
SplitObjs       = NO
NoFibWays =
STRIP=:

