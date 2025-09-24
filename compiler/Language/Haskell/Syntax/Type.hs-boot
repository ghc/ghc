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
{-
type HsMultAnn pass = HsMultAnnOf (LHsType (NoGhcTc pass)) pass

data HsMultAnnOf mult pass
  = HsUnannotated !(XUnannotated mult pass)
  | HsLinearAnn !(XLinearAnn mult pass)
  | HsExplicitMult !(XExplicitMult mult pass) !mult
  | XMultAnnOf !(XXMultAnnOf mult pass)

type family XUnannotated  mult p
type family XLinearAnn    mult p
type family XExplicitMult mult p
type family XXMultAnnOf   mult p

type LHsType pass = XRec pass (HsType pass)
-}