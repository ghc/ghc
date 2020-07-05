module GHC.CmmToAsm.ARM.Cond
  ( Cond (..)
  , condUnsigned
  ) where

import GHC.Prelude

-- | ARM Cond codes
data Cond
  = AL
  | CC
  -- ^ Also LO, unsigned lower
  | CS
  -- ^ Also HS, unsigned higher
  | EQ
  | GE
  | GT
  | HI
  | LE
  | LS
  | LT
  | MI
  | NE
  | PL
  | VC
  | VS
  deriving Eq

condUnsigned :: Cond -> Bool
condUnsigned CC  = True
condUnsigned CS  = True
condUnsigned HI  = True
condUnsigned LS  = True
condUnsigned _   = False
