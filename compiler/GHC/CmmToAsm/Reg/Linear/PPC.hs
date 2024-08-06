-- | Free regs map for PowerPC
module GHC.CmmToAsm.Reg.Linear.PPC where

import GHC.Prelude

import GHC.CmmToAsm.PPC.Regs
import GHC.Platform.Reg.Class.Unified
import GHC.Platform.Reg

import GHC.Utils.Outputable
import GHC.Platform

import Data.Word

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

instance Outputable FreeRegs where
    ppr = text . show

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0 0

releaseReg :: RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle r) (FreeRegs g f)
    | r > 31    = FreeRegs g (f .|. (1 `shiftL` (r - 32)))
    | otherwise = FreeRegs (g .|. (1 `shiftL` r)) f

initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

getFreeRegs :: RegClass -> FreeRegs -> [RealReg]        -- lazily
getFreeRegs cls (FreeRegs g f) =
    case cls of
      RcFloatOrVector -> go f (0x80000000) 63
      RcInteger       -> go g (0x80000000) 31
    where
        go _ 0 _ = []
        go x m i | x .&. m /= 0 = RealRegSingle i : (go x (m `shiftR` 1) $! i-1)
                 | otherwise    = go x (m `shiftR` 1) $! i-1

allocateReg :: RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs g f)
    | r > 31    = FreeRegs g (f .&. complement (1 `shiftL` (r - 32)))
    | otherwise = FreeRegs (g .&. complement (1 `shiftL` r)) f

