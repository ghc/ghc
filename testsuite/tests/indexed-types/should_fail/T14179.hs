{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T14179 where

data family   Foo1 a
data instance Foo1 a
data instance Foo1 a

data family   Foo2 :: k -> *
data instance Foo2 :: * -> *
data instance Foo2 :: * -> *

data family Foo3 a
data instance Foo3 [a] where
  Foo3a :: Foo3 [Int]
  Foo3b :: Foo3 [Bool]
data instance Foo3 [a] where
  Foo3c :: Foo3 [a]
  Foo3d :: Foo3 [Char]
