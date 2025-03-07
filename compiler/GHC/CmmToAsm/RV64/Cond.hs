-- To keep the style consistent: Please format this file with Ormolu
-- (https://github.com/tweag/ormolu).
module GHC.CmmToAsm.RV64.Cond
  ( Cond (..),
  )
where

import GHC.Prelude hiding (EQ)

-- | Condition codes.
--
-- Used in conditional branches and bit setters. According to the available
-- instruction set, some conditions are encoded as their negated opposites. I.e.
-- these are logical things that don't necessarily map 1:1 to hardware/ISA.
data Cond
  = -- | int and float
    EQ
  | -- | int and float
    NE
  | -- | signed less than
    SLT
  | -- | signed less than or equal
    SLE
  | -- | signed greater than or equal
    SGE
  | -- | signed greater than
    SGT
  | -- | unsigned less than
    ULT
  | -- | unsigned less than or equal
    ULE
  | -- | unsigned greater than or equal
    UGE
  | -- | unsigned greater than
    UGT
  | -- | floating point instruction @flt@
    FLT
  | -- | floating point instruction @fle@
    FLE
  | -- | floating point instruction @fge@
    FGE
  | -- | floating point instruction @fgt@
    FGT
  deriving (Eq, Show)
