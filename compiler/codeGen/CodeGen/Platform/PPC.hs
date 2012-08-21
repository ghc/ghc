
module CodeGen.Platform.PPC (callerSaves) where

import CmmExpr

#define MACHREGS_NO_REGS 0
#define MACHREGS_powerpc 1
#include "../../../../includes/CallerSaves.part.hs"

