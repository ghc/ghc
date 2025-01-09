module GHC.CmmToAsm.LA64.Cond where

import GHC.Prelude hiding (EQ)

-- | Condition codes.
-- Used in conditional branches and bit setters. According to the available
-- instruction set, some conditions are encoded as their negated opposites. I.e.
-- these are logical things that don't necessarily map 1:1 to hardware/ISA.
-- TODO: Maybe need to simplify or expand?
data Cond
-- ISA condition
  = EQ  -- beq
  | NE  -- bne
  | LT  -- blt
  | GE  -- bge
  | LTU  -- bltu
  | GEU  -- bgeu
  | EQZ  -- beqz
  | NEZ  -- bnez
-- Extra Logical condition
  | SLT -- LT
  | SLE
  | SGE -- GE
  | SGT
  | ULT -- LTU
  | ULE
  | UGE -- GEU
  | UGT
  | FLT
  | FLE
  | FGE
  | FGT
  deriving (Eq, Show)
