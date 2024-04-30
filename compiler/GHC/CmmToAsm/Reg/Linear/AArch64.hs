module GHC.CmmToAsm.Reg.Linear.AArch64 where

import GHC.Prelude

import GHC.CmmToAsm.AArch64.Regs
import GHC.Platform.Reg.Class
import GHC.Platform.Reg

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc( HasDebugCallStack )
import GHC.Platform

import Data.Word

-- AArch64 has 32 64bit general purpose register r0..r30, and zr/sp
-- AArch64 has 32 128bit floating point registers v0..v31 as part of the NEON
-- extension in Armv8-A.
--
-- Armv8-A is a fundamental change to the Arm architecture. It supports the
-- 64-bit Execution state called “AArch64”, and a new 64-bit instruction set
-- “A64”. To provide compatibility with the Armv7-A (32-bit architecture)
-- instruction set, a 32-bit variant of Armv8-A “AArch32” is provided. Most of
-- existing Armv7-A code can be run in the AArch32 execution state of Armv8-A.
--
-- these can be addresses as q/d/s/h/b 0..31, or v.f<size>[idx]
-- where size is 64, 32, 16, 8, ... and the index i allows us
-- to access the given part.
--
-- History of Arm Adv SIMD
-- .---------------------------------------------------------------------------.
-- | Armv6                  | Armv7-A                | Armv8-A AArch64         |
-- | SIMD extension         | NEON                   | NEON                    |
-- |===========================================================================|
-- | - Operates on 32-bit   | - Separate reg. bank,  | - Separate reg. bank,   |
-- |   GP ARM registers     |    32x64-bit NEON regs |   32x128-bit NEON regs  |
-- | - 8-bit/16-bit integer | - 8/16/32/64-bit int   | - 8/16/32/64-bit int    |
-- |                        | - Single precision fp  | - Single precision fp   |
-- |                        |                        | - Double precision fp   |
-- |                        |                        | - Single/Double fp are  |
-- |                        |                        |   IEEE compliant        |
-- | - 2x16-bit/4x8-bit ops | - Up to 16x8-bit ops   | - Up to 16x8-bit ops    |
-- |   per instruction      |   per instruction      |   per instruction       |
-- '---------------------------------------------------------------------------'

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
allocateReg :: HasDebugCallStack => RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs g f)
    | r > 31 && testBit f (r - 32) = FreeRegs g (clearBit f (r - 32))
    | r < 32 && testBit g r = FreeRegs (clearBit g r) f
    | r > 31 = panic $ "Linear.AArch64.allocReg: double allocation of float reg v" ++ show (r - 32) ++ "; " ++ showBits f
    | otherwise = pprPanic "Linear.AArch64.allocReg" $ text ("double allocation of gp reg x" ++ show r ++ "; " ++ showBits g)

-- we start from 28 downwards... the logic is similar to the ppc logic.
-- 31 is Stack Pointer
-- 30 is Link Register
-- 29 is Stack Frame (by convention)
-- 19-28 are callee save
-- the lower ones are all caller save

-- For this reason someone decided to give aarch64 only 6 regs for
-- STG:
-- 19: Base
-- 20: Sp
-- 21: Hp
-- 22-27: R1-R6
-- 28: SpLim

-- For LLVM code gen interop:
-- See https://lists.llvm.org/pipermail/llvm-commits/Week-of-Mon-20150119/253722.html
-- and the current ghccc implementation here:
-- https://github.com/llvm/llvm-project/blob/161ae1f39816edf667aaa190bce702a86879c7bd/llvm/lib/Target/AArch64/AArch64CallingConvention.td#L324-L363
-- and https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/generated-code
-- for the STG discussion.
{- For reference the ghcc from the link above:
let Entry = 1 in
def CC_AArch64_GHC : CallingConv<[
  CCIfType<[iPTR], CCBitConvertToType<i64>>,

  // Handle all vector types as either f64 or v2f64.
  CCIfType<[v1i64, v2i32, v4i16, v8i8, v2f32], CCBitConvertToType<f64>>,
  CCIfType<[v2i64, v4i32, v8i16, v16i8, v4f32, f128], CCBitConvertToType<v2f64>>,

  CCIfType<[v2f64], CCAssignToReg<[Q4, Q5]>>,
  CCIfType<[f32], CCAssignToReg<[S8, S9, S10, S11]>>,
  CCIfType<[f64], CCAssignToReg<[D12, D13, D14, D15]>>,

  // Promote i8/i16/i32 arguments to i64.
  CCIfType<[i8, i16, i32], CCPromoteToType<i64>>,

  // Pass in STG registers: Base, Sp, Hp, R1, R2, R3, R4, R5, R6, SpLim
  CCIfType<[i64], CCAssignToReg<[X19, X20, X21, X22, X23, X24, X25, X26, X27, X28]>>
]>;
-}

getFreeRegs :: RegClass -> FreeRegs -> [RealReg]
getFreeRegs cls (FreeRegs g f)
  | RcFloat   <- cls = [] -- For now we only support double and integer registers, floats will need to be promoted.
  | RcDouble  <- cls = go 32 f 31
  | RcInteger <- cls = go  0 g 18
    where
        go _   _ i | i < 0 = []
        go off x i | testBit x i = RealRegSingle (off + i) : (go off x $! i - 1)
                   | otherwise   = go off x $! i - 1

initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

releaseReg :: HasDebugCallStack => RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle r) (FreeRegs g f)
  | r > 31 && testBit f (r - 32) = pprPanic "Linear.AArch64.releaseReg" (text  "can't release non-allocated reg v" <> int (r - 32))
  | r < 32 && testBit g r = pprPanic "Linear.AArch64.releaseReg" (text "can't release non-allocated reg x" <> int r)
  | r > 31 = FreeRegs g (setBit f (r - 32))
  | otherwise = FreeRegs (setBit g r) f
