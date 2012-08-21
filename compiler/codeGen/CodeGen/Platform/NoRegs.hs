
module CodeGen.Platform.NoRegs (callerSaves) where

import CmmExpr

#define MACHREGS_NO_REGS 1
#include "../../../../includes/CallerSaves.part.hs"

