
module CodeGen.Platform.X86_64 (callerSaves) where

import CmmExpr

#define MACHREGS_NO_REGS 0
#define MACHREGS_x86_64 1
#include "../../../../includes/CallerSaves.part.hs"

