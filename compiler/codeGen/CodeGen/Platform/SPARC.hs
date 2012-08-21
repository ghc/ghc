
module CodeGen.Platform.SPARC (callerSaves) where

import CmmExpr

#define MACHREGS_NO_REGS 0
#define MACHREGS_sparc 1
#include "../../../../includes/CallerSaves.part.hs"

