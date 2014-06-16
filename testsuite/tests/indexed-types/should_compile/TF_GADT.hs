{-# LANGUAGE GADTs, TypeFamilies #-}

module TF_GADT where

-- Check that type families can be declared in GADT syntax
-- and indeed *be* GADTs

data family T a

data instance T [a] where
  T1 :: a -> T [a]


data instance T (Maybe a) where
  T3 :: Int -> T (Maybe Int)
  T4 :: a -> b -> T (Maybe (a,b))


f :: a -> T (Maybe a) -> T (Maybe a)
f x (T3 i) = T3 x
f x (T4 p q) = T4 p (snd x)
