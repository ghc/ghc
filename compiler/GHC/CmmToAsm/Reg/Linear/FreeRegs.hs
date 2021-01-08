{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.CmmToAsm.Reg.Linear.FreeRegs (
    FR(..),
    maxSpillSlots
)

#include "HsVersions.h"

where

import GHC.Prelude

import GHC.Platform.Reg
import GHC.Platform.Reg.Class

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

import qualified GHC.CmmToAsm.Reg.Linear.PPC    as PPC
import qualified GHC.CmmToAsm.Reg.Linear.SPARC  as SPARC
import qualified GHC.CmmToAsm.Reg.Linear.X86Base as X86Base

import qualified GHC.CmmToAsm.PPC.Instr   as PPC.Instr
import qualified GHC.CmmToAsm.SPARC.Instr as SPARC.Instr
import qualified GHC.CmmToAsm.X86.Instr   as X86.Instr

class Show freeRegs => FR freeRegs where
    frAllocateReg :: Platform -> RealReg -> freeRegs -> freeRegs
    frGetFreeRegs :: Platform -> RegClass -> freeRegs -> [RealReg]
    frInitFreeRegs :: Platform -> freeRegs
    frReleaseReg :: Platform -> RealReg -> freeRegs -> freeRegs

instance FR (X86Base.FreeRegsX86) where
    {-# INLINEABLE frAllocateReg #-}
    frAllocateReg  = \_ -> X86Base.allocateReg
    frGetFreeRegs  = X86Base.getFreeRegs
    frInitFreeRegs = X86Base.initFreeRegs
    frReleaseReg   = \_ -> X86Base.releaseReg

instance FR (X86Base.FreeRegsX86_64) where
    {-# INLINEABLE frAllocateReg #-}
    frAllocateReg  = \_ -> X86Base.allocateReg
    frGetFreeRegs  = X86Base.getFreeRegs
    frInitFreeRegs = X86Base.initFreeRegs
    frReleaseReg   = \_ -> X86Base.releaseReg

instance FR PPC.FreeRegs where
    frAllocateReg  = \_ -> PPC.allocateReg
    frGetFreeRegs  = \_ -> PPC.getFreeRegs
    frInitFreeRegs = PPC.initFreeRegs
    frReleaseReg   = \_ -> PPC.releaseReg

instance FR SPARC.FreeRegs where
    frAllocateReg  = SPARC.allocateReg
    frGetFreeRegs  = \_ -> SPARC.getFreeRegs
    frInitFreeRegs = SPARC.initFreeRegs
    frReleaseReg   = SPARC.releaseReg

maxSpillSlots :: NCGConfig -> Int
maxSpillSlots config = case platformArch (ncgPlatform config) of
   ArchX86       -> X86.Instr.maxSpillSlots config
   ArchX86_64    -> X86.Instr.maxSpillSlots config
   ArchPPC       -> PPC.Instr.maxSpillSlots config
   ArchS390X     -> panic "maxSpillSlots ArchS390X"
   ArchSPARC     -> SPARC.Instr.maxSpillSlots config
   ArchSPARC64   -> panic "maxSpillSlots ArchSPARC64"
   ArchARM _ _ _ -> panic "maxSpillSlots ArchARM"
   ArchAArch64   -> panic "maxSpillSlots ArchAArch64"
   ArchPPC_64 _  -> PPC.Instr.maxSpillSlots config
   ArchAlpha     -> panic "maxSpillSlots ArchAlpha"
   ArchMipseb    -> panic "maxSpillSlots ArchMipseb"
   ArchMipsel    -> panic "maxSpillSlots ArchMipsel"
   ArchRISCV64   -> panic "maxSpillSlots ArchRISCV64"
   ArchJavaScript-> panic "maxSpillSlots ArchJavaScript"
   ArchUnknown   -> panic "maxSpillSlots ArchUnknown"
