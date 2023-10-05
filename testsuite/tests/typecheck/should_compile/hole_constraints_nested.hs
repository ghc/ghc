{-# OPTIONS_GHC -fshow-hole-constraints #-}
{-# LANGUAGE GADTs, TypeOperators #-}
module HoleConstraintsNested where
import Data.Type.Equality

data EqOrd a where EqOrd :: (Eq a, Ord a) => EqOrd a

f :: a :~: b -> EqOrd a -> Int
f d1 d2 =
  case d1 of
    Refl -> case d2 of
      EqOrd -> _
