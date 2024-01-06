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

allMachRegNos   :: [RegNo]
allMachRegNos   = [0..31] ++ [32..63]
-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
-- i.e., these are the regs for which we are prepared to allow the
-- register allocator to attempt to map VRegs to.
allocatableRegs :: Platform -> [RealReg]
allocatableRegs platform
   = let isFree i = freeReg platform i
     in  map RealRegSingle $ filter isFree allMachRegNos

-- argRegs is the set of regs which are read for an n-argument call to C.
allGpArgRegs :: [Reg]
allGpArgRegs = map regSingle [10..17]
allFpArgRegs :: [Reg]
allFpArgRegs = map regSingle [42..49]

-- STG:
-- 19: Base
-- 20: Sp
-- 21: Hp
-- 22-27: R1-R6
-- 28: SpLim

-- This is the STG Sp reg.
-- sp :: Reg
-- sp = regSingle 20

-- addressing modes ------------------------------------------------------------

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
  | ImmLit      FastString
  | ImmIndex    CLabel Int
  | ImmFloat    Rational
  | ImmDouble   Rational
  | ImmConstantSum Imm Imm
  | ImmConstantDiff Imm Imm
  deriving (Eq, Show)

strImmLit :: FastString -> Imm
strImmLit s = ImmLit s


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
realRegSqueeze cls rr
 = case cls of
        RcInteger
         -> case rr of
                RealRegSingle regNo
                        | regNo < 32    -> 1     -- first fp reg is 32
                        | otherwise     -> 0

        RcDouble
         -> case rr of
                RealRegSingle regNo
                        | regNo < 32    -> 0
                        | otherwise     -> 1

        _other -> 0

mkVirtualReg :: Unique -> Format -> VirtualReg
mkVirtualReg u format
   | not (isFloatFormat format) = VirtualRegI u
   | otherwise
   = case format of
        FF32    -> VirtualRegD u
        FF64    -> VirtualRegD u
        _       -> panic "RV64.mkVirtualReg"

{-# INLINE classOfRealReg      #-}
classOfRealReg :: RealReg -> RegClass
classOfRealReg (RealRegSingle i)
        | i < 32        = RcInteger
        | otherwise     = RcDouble

regDotColor :: RealReg -> SDoc
regDotColor reg
 = case classOfRealReg reg of
        RcInteger       -> text "blue"
        RcFloat         -> text "red"
        RcDouble        -> text "green"
