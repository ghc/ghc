{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, EmptyCase #-}
module T10180 where

newtype Foo = Foo Int

type family Bar a
type instance Bar Int = Int

type family Baz a where
  Baz Int = Int
  Baz Char = Int

data a :~: b where
  Refl :: a :~: a

absurd0 :: Int :~: Bool -> a
absurd0 x = case x of {}

absurd1 :: Foo :~: Bool -> a
absurd1 x = case x of {}

absurd2 :: Bar Int :~: Bool -> a
absurd2 x = case x of {}

absurd3 :: Baz a :~: Bool -> a
absurd3 x = case x of {}

