-- | Free regs map for PowerPC
module RegAlloc.Linear.PPC.FreeRegs
where

import PPC.Regs
import RegClass
import Reg

import Outputable
import Platform

import Data.Word
import Data.Bits
-- import Data.List

-- The PowerPC has 32 integer and 32 floating point registers.
-- This is 32bit PowerPC, so Word64 is inefficient - two Word32s are much
-- better.
-- Note that when getFreeRegs scans for free registers, it starts at register
-- 31 and counts down. This is a hack for the PowerPC - the higher-numbered
-- registers are callee-saves, while the lower regs are caller-saves, so it
-- makes sense to start at the high end.
-- Apart from that, the code does nothing PowerPC-specific, so feel free to
-- add your favourite platform to the #if (if you have 64 registers but only
-- 32-bit words).

data FreeRegs = FreeRegs !Word32 !Word32
              deriving( Show )  -- The Show is used in an ASSERT

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0 0

releaseReg :: RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle r) (FreeRegs g f)
    | r > 31    = FreeRegs g (f .|. (1 `shiftL` (r - 32)))
    | otherwise = FreeRegs (g .|. (1 `shiftL` r)) f

releaseReg _ _
        = panic "RegAlloc.Linear.PPC.releaseReg: bad reg"
    
initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform = foldr releaseReg noFreeRegs (allocatableRegs platform)

getFreeRegs :: RegClass -> FreeRegs -> [RealReg]        -- lazily
getFreeRegs cls (FreeRegs g f)
    | RcDouble <- cls = go f (0x80000000) 63
    | RcInteger <- cls = go g (0x80000000) 31
    | otherwise = pprPanic "RegAllocLinear.getFreeRegs: Bad register class" (ppr cls)
    where
        go _ 0 _ = []
        go x m i | x .&. m /= 0 = RealRegSingle i : (go x (m `shiftR` 1) $! i-1)
                 | otherwise    = go x (m `shiftR` 1) $! i-1

allocateReg :: RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs g f) 
    | r > 31    = FreeRegs g (f .&. complement (1 `shiftL` (r - 32)))
    | otherwise = FreeRegs (g .&. complement (1 `shiftL` r)) f

allocateReg _ _
        = panic "RegAlloc.Linear.PPC.allocateReg: bad reg"
