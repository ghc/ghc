
WERROR          = -Werror

HADDOCK_DOCS    = YES
SRC_CC_OPTS     = $(WERROR)
SRC_HC_OPTS     = $(WERROR) -H64m -O0 -fasm
GhcStage1HcOpts = -O -fasm

GhcStage2HcOpts = -O -fasm
GhcLibHcOpts    = -O -fasm -dcore-lint
GhcLibWays      =
SplitObjs       = NO
NoFibWays       =
STRIP           = :
GhcBootLibs     = YES

ifeq "$(ValidateHpc)" "YES"
GhcStage2HcOpts += -fhpc -hpcdir $(FPTOOLS_TOP_ABS)/testsuite/hpc_output/
endif
ifeq "$(ValidateSlow)" "YES"
GhcStage2HcOpts += -XGenerics -DDEBUG
GhcLibHcOpts    += -XGenerics
endif

