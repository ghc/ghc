{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Free regs map for i386
module GHC.CmmToAsm.Reg.Linear.X86Base where

import GHC.Prelude

import GHC.CmmToAsm.X86.Regs
import GHC.Platform.Reg.Class
import GHC.Platform.Reg
import GHC.Utils.Panic
import GHC.Platform
import GHC.Utils.Outputable

import Data.Word
import Data.Bits

-- Instead of being parametric we could also just use Word64
-- at all times. Obviously bad for 32bit ghc performance. But
-- not sure if anyone still cares about that.
type FreeRegsX86    = FreeRegs Word32
type FreeRegsX86_64 = FreeRegs Word64

newtype FreeRegs w = FreeRegs w
    deriving (Show,Outputable)

type FreeRegConstraints w = (Bits w, Num w)

noFreeRegs :: FreeRegConstraints w => FreeRegs w
noFreeRegs = FreeRegs 0

{-# INLINEABLE releaseReg #-}
releaseReg :: FreeRegConstraints w => RealReg -> FreeRegs w -> FreeRegs w
releaseReg (RealRegSingle n) (FreeRegs f)
        = FreeRegs (f .|. (1 `shiftL` n))

releaseReg _ _
        = panic "RegAlloc.Linear.X86.FreeRegs.releaseReg: no reg"

{-# INLINEABLE initFreeRegs #-}
initFreeRegs :: FreeRegConstraints w =>  Platform -> FreeRegs w
initFreeRegs platform
        = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

{-# INLINEABLE getFreeRegs #-}
getFreeRegs :: FreeRegConstraints w =>  Platform -> RegClass -> FreeRegs w -> [RealReg] -- lazily
getFreeRegs platform cls (FreeRegs f) = go f 0

  where go 0 _ = []
        go n m
          -- Register is free && has the right class
          | n .&. 1 /= 0 && classOfRealReg platform (RealRegSingle m) == cls
          = RealRegSingle m : (go (n `shiftR` 1) $! (m+1))

          | otherwise
          = go (n `shiftR` 1) $! (m+1)
        -- ToDo: there's no point looking through all the integer registers
        -- in order to find a floating-point one.

{-# INLINEABLE allocateReg #-}
allocateReg :: FreeRegConstraints w =>  RealReg -> FreeRegs w -> FreeRegs w
allocateReg (RealRegSingle r) (FreeRegs f)
        = FreeRegs (f .&. complement (1 `shiftL` r))

allocateReg _ _
        = panic "RegAlloc.Linear.X86.FreeRegs.allocateReg: no reg"

