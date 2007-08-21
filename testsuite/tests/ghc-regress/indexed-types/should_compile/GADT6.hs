{-# LANGUAGE TypeFamilies, GADTs #-}

module GADT6 where

data Pair p  where
  Pair :: p~(a,b) => a -> b -> Pair p
  -- this works:
  -- Pair :: a -> b -> Pair (a,b)

foo :: Pair ((), ()) -> a
foo (Pair () ()) = undefined

