{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}
{-# LANGUAGE GADTs, TypeFamilies #-}

module T3927a where

type family F a
type instance F a = ()

data Foo a where
  FooA :: Foo ()
  FooB :: Foo Int

f :: a -> Foo (F a) -> () -- F a can only be () so only FooA is accepted
f _ FooA = ()

