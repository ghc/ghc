{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module T23145 where

data T a
  = MkT1 a
  | MkT2

f :: (forall a. T a) -> T b
f (MkT1 x) = MkT1 x
f MkT2 = MkT2
