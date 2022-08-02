{-# language MagicHash, TransformListComp, GADTs, TypeFamilies, PolyKinds, DataKinds #-}
{-# language PartialTypeSignatures, NamedWildCards, UnliftedNewtypes #-}

module T20864 where

import Data.Kind
import GHC.Exts

loom :: [Int] -> [Int]
loom xs = [z | I# x <- xs, let p = x +# 3#, z <- [1 .. I# p], then take 3]

type R :: Type -> RuntimeRep
type family R a where
  R Int = LiftedRep

class C a where
  type T a :: TYPE (R a)
  data D a :: Type
  cool :: [D a] -> [D a]

instance C Int where
  type T Int = Int
  data D Int = MkD (T Int)
  -- this one requires type-family reduction in the kind to observe liftedness
  cool :: [D Int] -> [D Int]
  cool xs = [MkD (x + 1612) | MkD x <- xs, then take 3]
