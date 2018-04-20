{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module T14934a where

import GHC.TypeLits

data Foo :: Nat -> * where
  MkFoo0 :: Foo 0
  MkFoo1 :: Foo 1

f :: Foo (1 - 0) -> Foo 1
f x = x
