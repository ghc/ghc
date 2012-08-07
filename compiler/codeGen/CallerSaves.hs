
module CallerSaves (callerSaves) where

import CmmExpr
import Platform

-- | Returns 'True' if this global register is stored in a caller-saves
-- machine register.

callerSaves :: Platform -> GlobalReg -> Bool
#define MACHREGS_NO_REGS 0
callerSaves (Platform { platformArch = ArchX86 }) = platformCallerSaves
  where
#define MACHREGS_i386 1
#include "../../includes/CallerSaves.part.hs"
#undef MACHREGS_i386
callerSaves (Platform { platformArch = ArchX86_64 }) = platformCallerSaves
  where
#define MACHREGS_x86_64 1
#include "../../includes/CallerSaves.part.hs"
#undef MACHREGS_x86_64
callerSaves (Platform { platformArch = ppcArch, platformOS = OSDarwin })
 | ppcArch `elem` [ArchPPC, ArchPPC_64] = platformCallerSaves
  where
#define MACHREGS_powerpc 1
#define MACHREGS_darwin 1
#include "../../includes/CallerSaves.part.hs"
#undef MACHREGS_powerpc
#undef MACHREGS_darwin
callerSaves (Platform { platformArch = ppcArch })
 | ppcArch `elem` [ArchPPC, ArchPPC_64] = platformCallerSaves
  where
#define MACHREGS_powerpc 1
#include "../../includes/CallerSaves.part.hs"
#undef MACHREGS_powerpc
callerSaves (Platform { platformArch = ArchSPARC }) = platformCallerSaves
  where
#define MACHREGS_sparc 1
#include "../../includes/CallerSaves.part.hs"
#undef MACHREGS_sparc
callerSaves (Platform { platformArch = ArchARM {} }) = platformCallerSaves
  where
#define MACHREGS_arm 1
#include "../../includes/CallerSaves.part.hs"
#undef MACHREGS_arm
callerSaves _ = platformCallerSaves
  where
#undef MACHREGS_NO_REGS
#define MACHREGS_NO_REGS 1
#include "../../includes/CallerSaves.part.hs"

