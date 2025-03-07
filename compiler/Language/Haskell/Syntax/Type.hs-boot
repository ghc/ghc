module Language.Haskell.Syntax.Type where

import Data.Bool
import Data.Eq
import Data.Ord

import Control.DeepSeq

{-
************************************************************************
*                                                                      *
\subsection{Promotion flag}
*                                                                      *
************************************************************************
-}

-- | Is a TyCon a promoted data constructor or just a normal type constructor?
data PromotionFlag
  = NotPromoted
  | IsPromoted

instance Eq PromotionFlag
instance Ord PromotionFlag
instance NFData PromotionFlag

isPromoted :: PromotionFlag -> Bool
