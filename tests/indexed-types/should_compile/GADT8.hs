{-# LANGUAGE TypeFamilies, GADTs #-}

module GADT8 where

data Pair p  where
  Pair :: p~(a,b) => a -> b -> Pair p
  -- this works:
  -- Pair :: a -> b -> Pair (a,b)

foo :: Pair ((), ()) -> Pair ((), ())
foo (Pair x y) = Pair x y

