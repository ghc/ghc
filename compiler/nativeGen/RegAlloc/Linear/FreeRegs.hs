
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module RegAlloc.Linear.FreeRegs (
    FR(..),
    maxSpillSlots
)

#include "HsVersions.h"

where

import Reg
import RegClass

import Panic
import Platform

-- -----------------------------------------------------------------------------
-- The free register set
-- This needs to be *efficient*
-- Here's an inefficient 'executable specification' of the FreeRegs data type:
--
--	type FreeRegs = [RegNo]
--	noFreeRegs = 0
--	releaseReg n f = if n `elem` f then f else (n : f)
--	initFreeRegs = allocatableRegs
--	getFreeRegs cls f = filter ( (==cls) . regClass . RealReg ) f
--	allocateReg f r = filter (/= r) f

import qualified RegAlloc.Linear.PPC.FreeRegs   as PPC
import qualified RegAlloc.Linear.SPARC.FreeRegs as SPARC
import qualified RegAlloc.Linear.X86.FreeRegs   as X86

import qualified PPC.Instr
import qualified SPARC.Instr
import qualified X86.Instr

class Show freeRegs => FR freeRegs where
    frAllocateReg :: RealReg -> freeRegs -> freeRegs
    frGetFreeRegs :: RegClass -> freeRegs -> [RealReg]
    frInitFreeRegs :: freeRegs
    frReleaseReg :: RealReg -> freeRegs -> freeRegs

instance FR X86.FreeRegs where
    frAllocateReg  = X86.allocateReg
    frGetFreeRegs  = X86.getFreeRegs
    frInitFreeRegs = X86.initFreeRegs
    frReleaseReg   = X86.releaseReg

instance FR PPC.FreeRegs where
    frAllocateReg  = PPC.allocateReg
    frGetFreeRegs  = PPC.getFreeRegs
    frInitFreeRegs = PPC.initFreeRegs
    frReleaseReg   = PPC.releaseReg

instance FR SPARC.FreeRegs where
    frAllocateReg  = SPARC.allocateReg
    frGetFreeRegs  = SPARC.getFreeRegs
    frInitFreeRegs = SPARC.initFreeRegs
    frReleaseReg   = SPARC.releaseReg

maxSpillSlots :: Platform -> Int
maxSpillSlots platform
              = case platformArch platform of
                ArchX86       -> X86.Instr.maxSpillSlots True  -- 32bit
                ArchX86_64    -> X86.Instr.maxSpillSlots False -- not 32bit
                ArchPPC       -> PPC.Instr.maxSpillSlots
                ArchSPARC     -> SPARC.Instr.maxSpillSlots
                ArchARM _ _ _ -> panic "maxSpillSlots ArchARM"
                ArchPPC_64    -> panic "maxSpillSlots ArchPPC_64"
                ArchUnknown   -> panic "maxSpillSlots ArchUnknown"

