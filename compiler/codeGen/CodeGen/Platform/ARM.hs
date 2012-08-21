
module CodeGen.Platform.ARM (callerSaves) where

import CmmExpr

#define MACHREGS_NO_REGS 0
#define MACHREGS_arm 1
#include "../../../../includes/CallerSaves.part.hs"

