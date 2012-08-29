
module CodeGen.Platform
       (callerSaves, activeStgRegs, haveRegBase, globalRegMaybe, freeReg)
       where

import CmmExpr
import FastBool
import Platform
import Reg

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
 | platformUnregisterised platform = NoRegs.callerSaves
 | otherwise
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

-- | Here is where the STG register map is defined for each target arch.
-- The order matters (for the llvm backend anyway)! We must make sure to
-- maintain the order here with the order used in the LLVM calling conventions.
-- Note that also, this isn't all registers, just the ones that are currently
-- possbily mapped to real registers.
activeStgRegs :: Platform -> [GlobalReg]
activeStgRegs platform
 | platformUnregisterised platform = NoRegs.activeStgRegs
 | otherwise
 = case platformArch platform of
   ArchX86    -> X86.activeStgRegs
   ArchX86_64 -> X86_64.activeStgRegs
   ArchSPARC  -> SPARC.activeStgRegs
   ArchARM {} -> ARM.activeStgRegs
   arch
    | arch `elem` [ArchPPC, ArchPPC_64] ->
       case platformOS platform of
       OSDarwin -> PPC_Darwin.activeStgRegs
       _        -> PPC.activeStgRegs

    | otherwise -> NoRegs.activeStgRegs

haveRegBase :: Platform -> Bool
haveRegBase platform
 | platformUnregisterised platform = NoRegs.haveRegBase
 | otherwise
 = case platformArch platform of
   ArchX86    -> X86.haveRegBase
   ArchX86_64 -> X86_64.haveRegBase
   ArchSPARC  -> SPARC.haveRegBase
   ArchARM {} -> ARM.haveRegBase
   arch
    | arch `elem` [ArchPPC, ArchPPC_64] ->
       case platformOS platform of
       OSDarwin -> PPC_Darwin.haveRegBase
       _        -> PPC.haveRegBase

    | otherwise -> NoRegs.haveRegBase

globalRegMaybe :: Platform -> GlobalReg -> Maybe RealReg
globalRegMaybe platform
 | platformUnregisterised platform = NoRegs.globalRegMaybe
 | otherwise
 = case platformArch platform of
   ArchX86    -> X86.globalRegMaybe
   ArchX86_64 -> X86_64.globalRegMaybe
   ArchSPARC  -> SPARC.globalRegMaybe
   ArchARM {} -> ARM.globalRegMaybe
   arch
    | arch `elem` [ArchPPC, ArchPPC_64] ->
       case platformOS platform of
       OSDarwin -> PPC_Darwin.globalRegMaybe
       _        -> PPC.globalRegMaybe

    | otherwise -> NoRegs.globalRegMaybe

freeReg :: Platform -> RegNo -> FastBool
freeReg platform
 | platformUnregisterised platform = NoRegs.freeReg
 | otherwise
 = case platformArch platform of
   ArchX86    -> X86.freeReg
   ArchX86_64 -> X86_64.freeReg
   ArchSPARC  -> SPARC.freeReg
   ArchARM {} -> ARM.freeReg
   arch
    | arch `elem` [ArchPPC, ArchPPC_64] ->
       case platformOS platform of
       OSDarwin -> PPC_Darwin.freeReg
       _        -> PPC.freeReg

    | otherwise -> NoRegs.freeReg

