{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

-- | Free regs map for i386
module GHC.CmmToAsm.Reg.Linear.X86 where

import GHC.Prelude

import GHC.CmmToAsm.X86.Regs
import GHC.Platform.Reg.Class
import GHC.Platform.Reg
import GHC.Platform
import GHC.Utils.Outputable
import qualified GHC.Types.BitSet as BitSet

import Data.Word

newtype FreeRegs = FreeRegs (BitSet.BitSet 32)
    deriving (Show,Outputable)

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs BitSet.empty

releaseReg :: RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle n) (FreeRegs f)
        = FreeRegs (f .|. (1 `shiftL` n))

initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform
        = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

integerRegs :: FreeRegs
integerRegs =
    FreeRegs $ BitSet.fromList
    [ classOfRealReg platform (RealRegSingle m) == RcInteger
    | m <- [0..32]
    ]

getFreeRegs :: Platform -> RegClass -> FreeRegs -> [RealReg] -- lazily
getFreeRegs platform cls (FreeRegs f) = BitSet. go f 0

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

