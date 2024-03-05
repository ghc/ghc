module GHC.CmmToAsm.RV64.Regs where

import GHC.Prelude
import GHC.Data.FastString

import GHC.Platform.Reg
import GHC.Platform.Reg.Class
import GHC.CmmToAsm.Format

import GHC.Cmm
import GHC.Cmm.CLabel           ( CLabel )
import GHC.Types.Unique

import GHC.Platform.Regs
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform

-- | First integer register number. @zero@ register.
x0RegNo :: RegNo
x0RegNo = 0

-- | Last integer register number. Used as IP register.
x31RegNo :: RegNo
x31RegNo = 31

-- | First floating point register.
d0RegNo :: RegNo
d0RegNo = 32

-- | Last floating point register.
d31RegNo :: RegNo
d31RegNo = 63

a0RegNo :: RegNo
a0RegNo = 10

a7RegNo :: RegNo
a7RegNo = 17

fa0RegNo :: RegNo
fa0RegNo = 42

fa7RegNo :: RegNo
fa7RegNo = 49

-- | All machine register numbers.
allMachRegNos :: [RegNo]
allMachRegNos = intRegs ++ fpRegs
  where
    intRegs = [x0RegNo .. x31RegNo]
    fpRegs = [d0RegNo .. d31RegNo]

-- | Registers available to the register allocator.
--
-- These are all registers minus those with a fixed role in RISCV ABI (zero, lr,
-- sp, gp, tp, fp, ip) and GHC RTS (Base, Sp, Hp, HpLim, R1..R8, F1..F6,
-- D1..D6.)
allocatableRegs :: Platform -> [RealReg]
allocatableRegs platform =
  let isFree = freeReg platform
   in map RealRegSingle $ filter isFree allMachRegNos

-- | Integer argument registers according to the calling convention
allGpArgRegs :: [Reg]
allGpArgRegs = map regSingle [a0RegNo .. a7RegNo]

-- | Floating point argument registers according to the calling convention
allFpArgRegs :: [Reg]
allFpArgRegs = map regSingle [fa0RegNo .. fa7RegNo]

-- addressing modes ------------------------------------------------------------

-- TODO: AddrRegReg constructor is never used.  Remove it?
data AddrMode
        = AddrRegReg    Reg Reg
        | AddrRegImm    Reg Imm
        | AddrReg       Reg
        deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- Immediates

data Imm
  = ImmInt      Int
  | ImmInteger  Integer     -- Sigh.
  | ImmCLbl     CLabel      -- AbstractC Label (with baggage)
  | ImmLit      FastString -- TODO: unused?
  | ImmIndex    CLabel Int
  | ImmFloat    Rational
  | ImmDouble   Rational
  | ImmConstantSum Imm Imm -- TODO: unused?
  | ImmConstantDiff Imm Imm  -- TODO: unused?
  deriving (Eq, Show)

litToImm :: CmmLit -> Imm
litToImm (CmmInt i w)        = ImmInteger (narrowS w i)
                -- narrow to the width: a CmmInt might be out of
                -- range, but we assume that ImmInteger only contains
                -- in-range values.  A signed value should be fine here.
litToImm (CmmFloat f W32)    = ImmFloat f
litToImm (CmmFloat f W64)    = ImmDouble f
litToImm (CmmLabel l)        = ImmCLbl l
litToImm (CmmLabelOff l off) = ImmIndex l off
litToImm (CmmLabelDiffOff l1 l2 off _)
                             = ImmConstantSum
                               (ImmConstantDiff (ImmCLbl l1) (ImmCLbl l2))
                               (ImmInt off)
-- TODO: Mention which CmmLit constructor didn't match.
litToImm _                   = panic "RV64.Regs.litToImm: no match"


-- == To satisfy GHC.CmmToAsm.Reg.Target =======================================

-- squeese functions for the graph allocator -----------------------------------
-- | regSqueeze_class reg
--      Calculate the maximum number of register colors that could be
--      denied to a node of this class due to having this reg
--      as a neighbour.
--
{-# INLINE virtualRegSqueeze #-}
virtualRegSqueeze :: RegClass -> VirtualReg -> Int
virtualRegSqueeze cls vr
 = case cls of
        RcInteger
         -> case vr of
                VirtualRegI{}           -> 1
                VirtualRegHi{}          -> 1
                _other                  -> 0

        RcDouble
         -> case vr of
                VirtualRegD{}           -> 1
                VirtualRegF{}           -> 0
                _other                  -> 0

        _other -> 0

{-# INLINE realRegSqueeze #-}
realRegSqueeze :: RegClass -> RealReg -> Int
realRegSqueeze cls rr =
  case cls of
    RcInteger ->
      case rr of
        RealRegSingle regNo
          | regNo < d0RegNo -> 1
          | otherwise -> 0
    RcDouble ->
      case rr of
        RealRegSingle regNo
          | regNo < d0RegNo -> 0
          | otherwise -> 1
    _other -> 0

mkVirtualReg :: Unique -> Format -> VirtualReg
mkVirtualReg u format
   | not (isFloatFormat format) = VirtualRegI u
   | otherwise
   = case format of
        FF32    -> VirtualRegD u
        FF64    -> VirtualRegD u
        _       -> panic "RV64.mkVirtualReg"

{-# INLINE classOfRealReg #-}
classOfRealReg :: RealReg -> RegClass
classOfRealReg (RealRegSingle i)
  | i < d0RegNo = RcInteger
  | otherwise = RcDouble

regDotColor :: RealReg -> SDoc
regDotColor reg
 = case classOfRealReg reg of
        RcInteger       -> text "blue"
        RcFloat         -> text "red"
        RcDouble        -> text "green"
