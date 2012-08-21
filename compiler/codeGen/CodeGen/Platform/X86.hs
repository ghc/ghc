
module CodeGen.Platform.X86 (callerSaves) where

import CmmExpr

#define MACHREGS_NO_REGS 0
#define MACHREGS_i386 1
#include "../../../../includes/CallerSaves.part.hs"

