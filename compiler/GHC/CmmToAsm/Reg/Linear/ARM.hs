module GHC.CmmToAsm.Reg.Linear.ARM where

import GHC.Prelude

import GHC.CmmToAsm.PPC.Regs
import GHC.Platform.Reg.Class
import GHC.Platform.Reg

import GHC.Utils.Outputable
import GHC.Platform

import Data.Word
import Data.Bits

-- ARM has 32 registers.
data FreeRegs = FreeRegs !Word32
              deriving( Show )  -- The Show is used in an ASSERT

instance Outputable FreeRegs where
    ppr = text . show

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0

releaseReg :: RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle r) (FreeRegs f)
    | r > 31    = FreeRegs (f .|. (1 `shiftL` (r - 32)))
    | otherwise = FreeRegs (f .|. (1 `shiftL` r))

releaseReg _ _
        = panic "RegAlloc.Linear.AR<.releaseReg: bad reg"

initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform =
  foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

getFreeRegs :: RegClass -> FreeRegs -> [RealReg]        -- lazily
getFreeRegs cls (FreeRegs f)
    | RcInteger <- cls = go f (0x80000000) 31
    | otherwise = pprPanic "RegAllocLinear.getFreeRegs: Bad register class" (ppr cls)
    where
        go _ 0 _ = []
        go x m i | x .&. m /= 0 = RealRegSingle i : (go x (m `shiftR` 1) $! i-1)
                 | otherwise    = go x (m `shiftR` 1) $! i-1

allocateReg :: RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs f)
    | r > 31    = FreeRegs (f .&. complement (1 `shiftL` (r - 32)))
    | otherwise = FreeRegs (f .&. complement (1 `shiftL` r))

allocateReg _ _
        = panic "RegAlloc.Linear.PPC.allocateReg: bad reg"
