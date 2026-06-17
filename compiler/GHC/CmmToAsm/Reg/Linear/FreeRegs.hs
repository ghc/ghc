module GHC.CmmToAsm.Reg.Linear.FreeRegs (
    FR(..),
    allFreeRegs,
    maxSpillSlots
)
where

import GHC.Prelude

import GHC.Platform.Reg
import GHC.Platform.Reg.Class
import qualified GHC.Platform.Reg.Class.Unified   as Unified
import qualified GHC.Platform.Reg.Class.Separate  as Separate
import qualified GHC.Platform.Reg.Class.NoVectors as NoVectors

import GHC.CmmToAsm.Config
import GHC.Utils.Panic
import GHC.Platform

-- -----------------------------------------------------------------------------
-- The free register set
-- This needs to be *efficient*
-- Here's an inefficient 'executable specification' of the FreeRegs data type:
--
--      type FreeRegs = [RegNo]
--      noFreeRegs = 0
--      releaseReg n f = if n `elem` f then f else (n : f)
--      initFreeRegs = allocatableRegs
--      getFreeRegs cls f = filter ( (==cls) . regClass . RealReg ) f
--      allocateReg f r = filter (/= r) f

import qualified GHC.CmmToAsm.Reg.Linear.PPC     as PPC
import qualified GHC.CmmToAsm.Reg.Linear.X86     as X86
import qualified GHC.CmmToAsm.Reg.Linear.X86_64  as X86_64
import qualified GHC.CmmToAsm.Reg.Linear.AArch64 as AArch64
import qualified GHC.CmmToAsm.Reg.Linear.RV64    as RV64
import qualified GHC.CmmToAsm.Reg.Linear.LA64    as LA64

import qualified GHC.CmmToAsm.PPC.Instr     as PPC.Instr
import qualified GHC.CmmToAsm.X86.Instr     as X86.Instr
import qualified GHC.CmmToAsm.AArch64.Instr as AArch64.Instr
import qualified GHC.CmmToAsm.RV64.Instr    as RV64.Instr
import qualified GHC.CmmToAsm.LA64.Instr    as LA64.Instr

class Show freeRegs => FR freeRegs where
    frAllocateReg :: RealReg -> freeRegs -> freeRegs
    frGetFreeRegs :: RegClass -> freeRegs -> [RealReg]
    -- | The initial allocatable set is platform-dependent. See Note
    -- [Aarch64 Register x18 at Darwin and Windows].
    frInitFreeRegs :: Platform -> freeRegs
    frReleaseReg :: RealReg -> freeRegs -> freeRegs

instance FR X86.FreeRegs where
    frAllocateReg  = X86.allocateReg
    frGetFreeRegs  = X86.getFreeRegs
    frInitFreeRegs = X86.initFreeRegs
    frReleaseReg   = X86.releaseReg

instance FR X86_64.FreeRegs where
    frAllocateReg  = X86_64.allocateReg
    frGetFreeRegs  = X86_64.getFreeRegs
    frInitFreeRegs = X86_64.initFreeRegs
    frReleaseReg   = X86_64.releaseReg

instance FR PPC.FreeRegs where
    frAllocateReg  = PPC.allocateReg
    frGetFreeRegs  = PPC.getFreeRegs
    frInitFreeRegs = PPC.initFreeRegs
    frReleaseReg   = PPC.releaseReg

instance FR AArch64.FreeRegs where
    frAllocateReg = AArch64.allocateReg
    frGetFreeRegs = AArch64.getFreeRegs
    frInitFreeRegs = AArch64.initFreeRegs
    frReleaseReg = AArch64.releaseReg

instance FR RV64.FreeRegs where
    frAllocateReg = RV64.allocateReg
    frGetFreeRegs = RV64.getFreeRegs
    frInitFreeRegs = RV64.initFreeRegs
    frReleaseReg = RV64.releaseReg

instance FR LA64.FreeRegs where
    frAllocateReg = LA64.allocateReg
    frGetFreeRegs = LA64.getFreeRegs
    frInitFreeRegs = LA64.initFreeRegs
    frReleaseReg = LA64.releaseReg

allFreeRegs :: FR freeRegs => Platform -> freeRegs -> [RealReg]
allFreeRegs plat fr = foldMap (\rcls -> frGetFreeRegs rcls fr) allRegClasses
  where
    allRegClasses =
      case registerArch (platformArch plat) of
        Unified   ->   Unified.allRegClasses
        Separate  ->  Separate.allRegClasses
        NoVectors -> NoVectors.allRegClasses

maxSpillSlots :: NCGConfig -> Int
maxSpillSlots config = case platformArch (ncgPlatform config) of
   ArchX86       -> X86.Instr.maxSpillSlots config
   ArchX86_64    -> X86.Instr.maxSpillSlots config
   ArchPPC       -> PPC.Instr.maxSpillSlots config
   ArchS390X     -> panic "maxSpillSlots ArchS390X"
   ArchARM _ _ _ -> panic "maxSpillSlots ArchARM"
   ArchAArch64   -> AArch64.Instr.maxSpillSlots config
   ArchPPC_64 _  -> PPC.Instr.maxSpillSlots config
   ArchAlpha     -> panic "maxSpillSlots ArchAlpha"
   ArchMipseb    -> panic "maxSpillSlots ArchMipseb"
   ArchMipsel    -> panic "maxSpillSlots ArchMipsel"
   ArchRISCV64   -> RV64.Instr.maxSpillSlots config
   ArchLoongArch64  -> LA64.Instr.maxSpillSlots config
   ArchJavaScript-> panic "maxSpillSlots ArchJavaScript"
   ArchWasm32    -> panic "maxSpillSlots ArchWasm32"
   ArchUnknown   -> panic "maxSpillSlots ArchUnknown"
