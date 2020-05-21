{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Free regs map for x86_64
module GHC.CmmToAsm.Reg.Linear.X86_64 where

import GHC.Prelude

import GHC.CmmToAsm.X86.Regs
import GHC.Platform.Reg.Class
import GHC.Platform.Reg
import GHC.Utils.Panic
import GHC.Platform
import GHC.Utils.Outputable

import Data.Word
import Data.Bits

newtype FreeRegs = FreeRegs Word64
    deriving (Show,Outputable)

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0

releaseReg :: RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle n) (FreeRegs f)
        = FreeRegs (f .|. (1 `shiftL` n))

releaseReg _ _
        = panic "RegAlloc.Linear.X86_64.FreeRegs.releaseReg: no reg"

initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform
        = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

getFreeRegs :: Platform -> RegClass -> FreeRegs -> [RealReg] -- lazily
getFreeRegs platform cls (FreeRegs f) = go f 0

  where go 0 _ = []
        go n m
          | n .&. 1 /= 0 && classOfRealReg platform (RealRegSingle m) == cls
          = RealRegSingle m : (go (n `shiftR` 1) $! (m+1))

          | otherwise
          = go (n `shiftR` 1) $! (m+1)
        -- ToDo: there's no point looking through all the integer registers
        -- in order to find a floating-point one.

allocateReg :: RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs f)
        = FreeRegs (f .&. complement (1 `shiftL` r))

allocateReg _ _
        = panic "RegAlloc.Linear.X86_64.FreeRegs.allocateReg: no reg"


