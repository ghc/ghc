{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Free regs map for i386
module GHC.CmmToAsm.Reg.Linear.X86 where

import GHC.Prelude

import GHC.CmmToAsm.X86.Regs
import GHC.Platform.Reg.Class.Unified
import GHC.Platform.Reg
import GHC.Platform
import GHC.Utils.Outputable

import Data.Word

newtype FreeRegs = FreeRegs Word32
    deriving (Show,Outputable)

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0

releaseReg :: RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle n) (FreeRegs f)
        = FreeRegs (setBit f n)

initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform
        = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

getFreeRegs :: Platform -> RegClass -> FreeRegs -> [RealReg] -- lazily
getFreeRegs platform cls (FreeRegs f) =
  case cls of
    RcInteger ->
      [ RealRegSingle i
      | i <- intregnos platform
      , testBit f i
      ]
    RcFloatOrVector ->
      [ RealRegSingle i
      | i <- xmmregnos platform
      , testBit f i
      ]

allocateReg :: RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs f)
        = FreeRegs (clearBit f r)

