module GHC.CmmToAsm.RV64.Regs where

import GHC.Cmm
import GHC.Cmm.CLabel (CLabel)
import GHC.CmmToAsm.Format
import GHC.Data.FastString
import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Reg.Class
import GHC.Platform.Reg.Class.Separate
import GHC.Platform.Regs
import GHC.Prelude
import GHC.Types.Unique
import GHC.Utils.Outputable
import GHC.Utils.Panic

-- * Registers

-- | First integer register number. @zero@ register.
x0RegNo :: RegNo
x0RegNo = 0

-- | return address register
x1RegNo, raRegNo :: RegNo
x1RegNo = 1
raRegNo = x1RegNo

x5RegNo, t0RegNo :: RegNo
x5RegNo = 5
t0RegNo = x5RegNo

x7RegNo, t2RegNo :: RegNo
x7RegNo = 7
t2RegNo = x7RegNo

x28RegNo, t3RegNo :: RegNo
x28RegNo = 28
t3RegNo = x28RegNo

-- | Last integer register number. Used as TMP (IP) register.
x31RegNo, t6RegNo, tmpRegNo :: RegNo
x31RegNo = 31
t6RegNo = x31RegNo
tmpRegNo = x31RegNo

-- | First floating point register.
d0RegNo, ft0RegNo :: RegNo
d0RegNo = 32
ft0RegNo = d0RegNo

d7RegNo, ft7RegNo :: RegNo
d7RegNo = 39
ft7RegNo = d7RegNo

-- | Last floating point register.
d31RegNo :: RegNo
d31RegNo = 63

a0RegNo, x10RegNo :: RegNo
x10RegNo = 10
a0RegNo = x10RegNo

a7RegNo, x17RegNo :: RegNo
x17RegNo = 17
a7RegNo = x17RegNo

fa0RegNo, d10RegNo :: RegNo
d10RegNo = 42
fa0RegNo = d10RegNo

fa7RegNo, d17RegNo :: RegNo
d17RegNo = 49
fa7RegNo = d17RegNo

-- Note [The made-up RISCV64 TMP (IP) register]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- RISCV64 has no inter-procedural register in its ABI. However, we need one to
-- make register spills/loads to/from high number slots. I.e. slot numbers that
-- do not fit in a 12bit integer which is used as immediate in the arithmetic
-- operations. Thus, we're marking one additional register (x31) as permanently
-- non-free and call it TMP.
--
-- TMP can be used as temporary register in all operations. Just be aware that
-- it may be clobbered as soon as you loose direct control over it (i.e. using
-- TMP by-passes the register allocation/spilling mechanisms.) It should be fine
-- to use it as temporary register in a MachOp translation as long as you don't
-- rely on its value beyond this limited scope.
--
-- X31 is a caller-saved register. I.e. there are no guarantees about what the
-- callee does with it. That's exactly what we want here.

zeroReg, raReg, spMachReg, tmpReg :: Reg
zeroReg = regSingle x0RegNo
raReg = regSingle 1

-- | Not to be confused with the `CmmReg` `spReg`
spMachReg = regSingle 2

tmpReg = regSingle tmpRegNo

-- | All machine register numbers.
allMachRegNos :: [RegNo]
allMachRegNos = intRegs ++ fpRegs
  where
    intRegs = [x0RegNo .. x31RegNo]
    fpRegs = [d0RegNo .. d31RegNo]

-- | Registers available to the register allocator.
--
-- These are all registers minus those with a fixed role in RISCV ABI (zero, lr,
-- sp, gp, tp, fp, tmp) and GHC RTS (Base, Sp, Hp, HpLim, R1..R8, F1..F6,
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

-- * Addressing modes

-- | Addressing modes
data AddrMode
  = -- | A register plus some immediate integer, e.g. @8(sp)@ or @-16(sp)@. The
    -- offset needs to fit into 12bits.
    AddrRegImm Reg Imm
  | -- | A register
    AddrReg Reg
  deriving (Eq, Show)

-- * Immediates

data Imm
  = ImmInt Int
  | ImmInteger Integer -- Sigh.
  | ImmCLbl CLabel -- AbstractC Label (with baggage)
  | ImmLit FastString
  | ImmIndex CLabel Int
  | ImmFloat Rational
  | ImmDouble Rational
  | ImmConstantSum Imm Imm
  | ImmConstantDiff Imm Imm
  deriving (Eq, Show)

-- | Map `CmmLit` to `Imm`
--
-- N.B. this is a partial function, because not all `CmmLit`s have an immediate
-- representation.
litToImm :: CmmLit -> Imm
litToImm (CmmInt i w) = ImmInteger (narrowS w i)
-- narrow to the width: a CmmInt might be out of
-- range, but we assume that ImmInteger only contains
-- in-range values.  A signed value should be fine here.
litToImm (CmmFloat f W32) = ImmFloat f
litToImm (CmmFloat f W64) = ImmDouble f
litToImm (CmmLabel l) = ImmCLbl l
litToImm (CmmLabelOff l off) = ImmIndex l off
litToImm (CmmLabelDiffOff l1 l2 off _) =
  ImmConstantSum
    (ImmConstantDiff (ImmCLbl l1) (ImmCLbl l2))
    (ImmInt off)
litToImm l = panic $ "RV64.Regs.litToImm: no match for " ++ show l

-- == To satisfy GHC.CmmToAsm.Reg.Target =======================================

-- squeese functions for the graph allocator -----------------------------------

-- | regSqueeze_class reg
--      Calculate the maximum number of register colors that could be
--      denied to a node of this class due to having this reg
--      as a neighbour.
{-# INLINE virtualRegSqueeze #-}
virtualRegSqueeze :: RegClass -> VirtualReg -> Int
virtualRegSqueeze cls vr =
  case cls of
    RcInteger ->
      case vr of
        VirtualRegI {} -> 1
        VirtualRegHi {} -> 1
        _other -> 0
    RcFloat ->
      case vr of
        VirtualRegD {} -> 1
        _other -> 0
    RcVector ->
      case vr of
        VirtualRegV128 {} -> 1
        _other -> 0

{-# INLINE realRegSqueeze #-}
realRegSqueeze :: RegClass -> RealReg -> Int
realRegSqueeze cls rr =
  case cls of
    RcInteger ->
      case rr of
        RealRegSingle regNo
          | regNo < d0RegNo
          -> 1
          | otherwise
          -> 0
    RcFloat ->
      case rr of
        RealRegSingle regNo
          |  regNo < d0RegNo
          || regNo > d31RegNo
          -> 0
          | otherwise
          -> 1
    RcVector ->
      case rr of
        RealRegSingle regNo
          | regNo > d31RegNo
          -> 1
          | otherwise
          -> 0

mkVirtualReg :: Unique -> Format -> VirtualReg
mkVirtualReg u format
  | not (isFloatFormat format) = VirtualRegI u
  | otherwise =
      case format of
        FF32 -> VirtualRegD u
        FF64 -> VirtualRegD u
        _ -> panic "RV64.mkVirtualReg"

{-# INLINE classOfRealReg #-}
classOfRealReg :: RealReg -> RegClass
classOfRealReg (RealRegSingle i)
  | i < d0RegNo = RcInteger
  | i > d31RegNo = RcVector
  | otherwise = RcFloat

regDotColor :: RealReg -> SDoc
regDotColor reg =
  case classOfRealReg reg of
    RcInteger -> text "blue"
    RcFloat -> text "red"
    RcVector -> text "green"
