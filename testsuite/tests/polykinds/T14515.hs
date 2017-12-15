{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
module Bug where

import Data.Kind

type HRank1 ty = forall k1. k1 -> ty
type HRank2 ty = forall k2. k2 -> ty

data HREFL11 :: HRank1 (HRank1 Type) -- FAILS
data HREFL12 :: HRank1 (HRank2 Type)
data HREFL21 :: HRank2 (HRank1 Type)
data HREFL22 :: HRank2 (HRank2 Type) -- FAILS
