module Language.Haskell.Syntax.Type where

import Data.Bool
import Data.Eq

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

isPromoted :: PromotionFlag -> Bool
