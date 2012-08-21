
module CodeGen.CallerSaves (callerSaves) where

import CmmExpr
import Platform

import qualified CodeGen.Platform.ARM        as ARM
import qualified CodeGen.Platform.PPC        as PPC
import qualified CodeGen.Platform.PPC_Darwin as PPC_Darwin
import qualified CodeGen.Platform.SPARC      as SPARC
import qualified CodeGen.Platform.X86        as X86
import qualified CodeGen.Platform.X86_64     as X86_64
import qualified CodeGen.Platform.NoRegs     as NoRegs

-- | Returns 'True' if this global register is stored in a caller-saves
-- machine register.

callerSaves :: Platform -> GlobalReg -> Bool
callerSaves platform
 = case platformArch platform of
   ArchX86    -> X86.callerSaves
   ArchX86_64 -> X86_64.callerSaves
   ArchSPARC  -> SPARC.callerSaves
   ArchARM {} -> ARM.callerSaves
   arch
    | arch `elem` [ArchPPC, ArchPPC_64] ->
       case platformOS platform of
       OSDarwin -> PPC_Darwin.callerSaves
       _        -> PPC.callerSaves

    | otherwise -> NoRegs.callerSaves

