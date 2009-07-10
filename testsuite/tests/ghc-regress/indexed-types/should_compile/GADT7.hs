{-# LANGUAGE TypeFamilies, GADTs #-}

module GADT7 where

data Pair p  where
  Pair :: p~(a,b) => a -> b -> Pair p
  -- this works:
--  Pair :: a -> b -> Pair (a,b)

foo :: a
foo = case Pair () () of
   -- this works:
--   case Pair () () :: Pair ((), ()) of
        Pair x y -> undefined

