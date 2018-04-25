{-# LANGUAGE DeriveAnyClass #-}
module T12144_2 where

class C1 a

instance C1 a => C1 (Foo a)

class C1 a => C2 a where
  c2 :: a -> String
  c2 _ = "C2 default"

newtype Foo a = Foo a deriving C2

foo :: C1 a => Foo a -> String
foo = c2
