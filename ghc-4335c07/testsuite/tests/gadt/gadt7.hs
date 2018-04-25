{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

-- From Yann Regis-Gianas at INRIA

module ShouldCompile where

data T a where
  K :: T Int

-- Should fail
i1 :: T a -> a -> Int
i1 t y = (\t1 y1 -> case t1 of K -> y1) t y

-- No type signature; should not type-check,
-- because we can't unify under the equalty constraint for K
i1b t y = (\t1 y1 -> case t1 of K -> y1) t y

i2 :: T a -> a -> Int
i2 t (y::b)  = case t of { K -> y+(1::Int) }

i3 :: forall a. T a -> a -> Int
i3 t y 
  = let t1 = t in
    let y1 = y in
    case t1 of K -> y1

i4 :: forall a. T a -> a -> Int
i4 (t :: T a) (y :: a)
  = let t1 = t in
    let y1 = y in
    case t1 of K -> y1




