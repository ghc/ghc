module GHC.CmmToAsm.Reg.Linear.LA64 where

import GHC.Prelude

import Data.Word
import GHC.CmmToAsm.LA64.Regs
import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Reg.Class
import GHC.Platform.Reg.Class.Separate
import GHC.Stack
import GHC.Utils.Outputable
import GHC.Utils.Panic

data FreeRegs = FreeRegs !Word32 !Word32

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0 0

instance Show FreeRegs where
  show (FreeRegs g f) = "FreeRegs 0b" ++ showBits g ++ " 0b" ++ showBits f

-- | Show bits as a `String` of @1@s and @0@s
showBits :: Word32 -> String
showBits w = map (\i -> if testBit w i then '1' else '0') [0 .. 31]

instance Outputable FreeRegs where
  ppr (FreeRegs g f) =
         text "   " <+> foldr (\i x -> pad_int i <+> x) (text "") [0 .. 31]
      $$ text "GPR" <+> foldr (\i x -> show_bit g i <+> x) (text "") [0 .. 31]
      $$ text "FPR" <+> foldr (\i x -> show_bit f i <+> x) (text "") [0 .. 31]
    where
      pad_int i | i < 10 = char ' ' <> int i
      pad_int i = int i
      -- remember bit = 1 means it's available.
      show_bit bits bit | testBit bits bit = text "  "
      show_bit _ _ = text " x"

-- | Set bits of all allocatable registers to 1
initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

-- | Get all free `RealReg`s (i.e. those where the corresponding bit is 1)
getFreeRegs :: RegClass -> FreeRegs -> [RealReg]
getFreeRegs cls (FreeRegs g f)
  | RcInteger <- cls = go 0 g allocatableIntRegs
  | RcFloat   <- cls = go 32 f allocatableDoubleRegs
  | RcVector  <- cls = sorry "Linear.LA64.getFreeRegs: vector registers are not supported"
  where
    go _ _ [] = []
    go off x (i : is)
      | testBit x i = RealRegSingle (off + i) : (go off x $! is)
      | otherwise = go off x $! is
    allocatableIntRegs = [4 .. 11] ++ [12 .. 19]
    allocatableDoubleRegs = [0 .. 7] ++ [8 .. 23]

-- | Set corresponding register bit to 0
allocateReg :: (HasCallStack) => RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs g f)
  | r > 31 && testBit f (r - 32) = FreeRegs g (clearBit f (r - 32))
  | r < 32 && testBit g r = FreeRegs (clearBit g r) f
  | r > 31 = panic $ "Linear.LA64.allocReg: double allocation of float reg v" ++ show (r - 32) ++ "; " ++ showBits f
  | otherwise = pprPanic "Linear.LA64.allocReg" $ text ("double allocation of gp reg x" ++ show r ++ "; " ++ showBits g)

-- | Set corresponding register bit to 1
releaseReg :: (HasCallStack) => RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle r) (FreeRegs g f)
  | r > 31 && testBit f (r - 32) = pprPanic "Linear.LA64.releaseReg" (text "can't release non-allocated reg v" <> int (r - 32))
  | r < 32 && testBit g r = pprPanic "Linear.LA64.releaseReg" (text "can't release non-allocated reg x" <> int r)
  | r > 31 = FreeRegs g (setBit f (r - 32))
  | otherwise = FreeRegs (setBit g r) f
