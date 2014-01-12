{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Foo( shared, foo, bar) where

import GHC.Exts

type family Domain (f :: * -> *) a :: Constraint

type instance Domain [] a = ()

instance MyFunctor [] where
  myfmap = map

class MyFunctor f where
  myfmap :: (Domain f a, Domain f b) => (a -> b) -> f a -> f b

shared :: (MyFunctor f, Domain f Int) => f Int -> f Int
shared = let
  f = myfmap negate
  in
  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.
  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.
  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.
  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.  f.
  f

foo xs = shared $ 0:xs
bar xs = 0:shared xs
