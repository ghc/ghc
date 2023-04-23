{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Free regs map for RISC-V 64bit
module GHC.CmmToAsm.Reg.Linear.RISCV64 where

import GHC.Prelude

import GHC.CmmToAsm.RISCV64.Regs
import GHC.Platform.Reg.Class
import GHC.Platform.Reg

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform

import Data.Word

import GHC.Stack

-- TODO: Register selection can likely be smart. Re-check this. (I'm not even sure this is correct.)
data FreeRegs = FreeRegs !Word32 !Word32

instance Show FreeRegs where
  show (FreeRegs g f) = "FreeRegs: " ++ showBits g ++ "; " ++ showBits f

instance Outputable FreeRegs where
    ppr (FreeRegs g f) = text "   " <+> foldr (\i x -> pad_int i    <+> x) (text "") [0..31]
                      $$ text "GPR" <+> foldr (\i x -> show_bit g i <+> x) (text "") [0..31]
                      $$ text "FPR" <+> foldr (\i x -> show_bit f i <+> x) (text "") [0..31]
      where pad_int i | i < 10 = char ' ' <> int i
            pad_int i = int i
            -- remember bit = 1 means it's available.
            show_bit bits bit | testBit bits bit = text "  "
            show_bit _    _ = text " x"

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0 0

showBits :: Word32 -> String
showBits w = map (\i -> if testBit w i then '1' else '0') [0..31]

-- FR instance implementation (See Linear.FreeRegs)
allocateReg :: HasCallStack => RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs g f)
    | r > 31 && testBit f (r - 32) = FreeRegs g (clearBit f (r - 32))
    | r < 32 && testBit g r = FreeRegs (clearBit g r) f
    | r > 31 = panic $ "Linear.RISCV64.allocReg: double allocation of float reg v" ++ show (r - 32) ++ "; " ++ showBits f
    | otherwise = pprPanic "Linear.RISCV64.allocReg" $ text ("double allocation of gp reg x" ++ show r ++ "; " ++ showBits g)

getFreeRegs :: RegClass -> FreeRegs -> [RealReg]
getFreeRegs cls (FreeRegs g f)
  -- TODO: how to handle small floats?
  | RcFloat   <- cls = [] -- For now we only support double and integer registers, floats will need to be promoted.
  | RcDouble  <- cls = go 32 f 31
  | RcInteger <- cls = go 1 g 30
    where
        go _   _ i | i < 0 = []
        go off x i | testBit x i = RealRegSingle (off + i) : (go off x $! i - 1)
                   | otherwise   = go off x $! i - 1

initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

releaseReg :: HasCallStack => RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle r) (FreeRegs g f)
  | r > 31 && testBit f (r - 32) = pprPanic "Linear.RISCV64.releaseReg" (text  "can't release non-allocated reg v" <> int (r - 32))
  | r < 32 && testBit g r = pprPanic "Linear.RISCV64.releaseReg" (text "can't release non-allocated reg x" <> int r <+> text (showBits g))
  | r > 31 = FreeRegs g (setBit f (r - 32))
  | otherwise = FreeRegs (setBit g r) f
