{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module T14934a where

import Data.Kind (Type)
import GHC.TypeLits

data Foo :: Nat -> Type where
  MkFoo0 :: Foo 0
  MkFoo1 :: Foo 1

f :: Foo (1 - 0) -> Foo 1
f x = x

g :: Foo (CharToNat '\1') -> Foo 1
g x = x
