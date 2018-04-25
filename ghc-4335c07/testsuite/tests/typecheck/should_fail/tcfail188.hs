{-# LANGUAGE RankNTypes, KindSignatures #-}
{-# OPTIONS_GHC -Werror #-}

-- Trac #959

module ShouldFail where

data D (f :: (* -> *) -> * -> *) (af :: * -> *) (ax :: *) =
  D (af (f af ax))

data CList (f :: (* -> *) -> * -> *) (a :: *) =
  RCons a (CList (D f) a)

type CycleList a = forall f. CList f a

chead :: CycleList a -> a
chead ys = case ys of (RCons x xs) -> x
