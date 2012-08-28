
module CodeGen.Platform.NoRegs where

import CmmExpr

#define MACHREGS_NO_REGS 1
#include "../../../../includes/CodeGen.Platform.hs"

