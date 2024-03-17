{-# OPTIONS -ddump-parsed-ast #-}
module Test24533 where

instance
  ( Read a, -- Weird
    Read b
  ) =>
  Read (a, b)

class Foo (a :: Type {- Weird -})

instance Eq Foo where
  -- Weird
  Foo == Foo = True
