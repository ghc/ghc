-- | Functions to implement the @FR@ (as in "free regs") type class.
--
-- For LLVM GHC calling convention (used registers), see
-- https://github.com/llvm/llvm-project/blob/6ab900f8746e7d8e24afafb5886a40801f6799f4/llvm/lib/Target/RISCV/RISCVISelLowering.cpp#L13638-L13685
module GHC.CmmToAsm.Reg.Linear.RV64
  ( allocateReg,
    getFreeRegs,
    initFreeRegs,
    releaseReg,
    FreeRegs (..),
  )
where

import Data.Word
import GHC.CmmToAsm.RV64.Regs
import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Reg.Class
import GHC.Prelude
import GHC.Stack
import GHC.Utils.Outputable
import GHC.Utils.Panic

data FreeRegs = FreeRegs !Word32 !Word32

instance Show FreeRegs where
  show (FreeRegs g f) = "FreeRegs 0b" ++ showBits g ++ " 0b" ++ showBits f

-- | Show bits as a `String` of @1@s and @0@s
showBits :: Word32 -> String
showBits w = map (\i -> if testBit w i then '1' else '0') [0 .. 31]

instance Outputable FreeRegs where
  ppr (FreeRegs g f) =
    text "   "
      <+> foldr (\i x -> pad_int i <+> x) (text "") [0 .. 31]
      $$ text "GPR"
      <+> foldr (\i x -> show_bit g i <+> x) (text "") [0 .. 31]
      $$ text "FPR"
      <+> foldr (\i x -> show_bit f i <+> x) (text "") [0 .. 31]
    where
      pad_int i | i < 10 = char ' ' <> int i
      pad_int i = int i
      -- remember bit = 1 means it's available.
      show_bit bits bit | testBit bits bit = text "  "
      show_bit _ _ = text " x"

initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)
  where
    noFreeRegs :: FreeRegs
    noFreeRegs = FreeRegs 0 0

getFreeRegs :: RegClass -> FreeRegs -> [RealReg]
getFreeRegs cls (FreeRegs g f)
  | RcFloat <- cls = [] -- For now we only support double and integer registers, floats will need to be promoted.
  | RcDouble <- cls = go 32 f [0 .. 31]
  | RcInteger <- cls = go 0 g ([5 .. 7] ++ [10 .. 17] ++ [28 .. 31])
  where
    go _ _ [] = []
    go off x (i : is)
      | testBit x i = RealRegSingle (off + i) : (go off x $! is)
      | otherwise = go off x $! is

allocateReg :: (HasCallStack) => RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs g f)
  | r > 31 && testBit f (r - 32) = FreeRegs g (clearBit f (r - 32))
  | r < 32 && testBit g r = FreeRegs (clearBit g r) f
  | r > 31 = panic $ "Linear.RV64.allocReg: double allocation of float reg v" ++ show (r - 32) ++ "; " ++ showBits f
  | otherwise = pprPanic "Linear.RV64.allocReg" $ text ("double allocation of gp reg x" ++ show r ++ "; " ++ showBits g)

releaseReg :: (HasCallStack) => RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle r) (FreeRegs g f)
  | r > 31 && testBit f (r - 32) = pprPanic "Linear.RV64.releaseReg" (text "can't release non-allocated reg v" <> int (r - 32))
  | r < 32 && testBit g r = pprPanic "Linear.RV64.releaseReg" (text "can't release non-allocated reg x" <> int r)
  | r > 31 = FreeRegs g (setBit f (r - 32))
  | otherwise = FreeRegs (setBit g r) f
