{-# OPTIONS -ddump-parsed-ast #-}
module Test24533 where

instance
  ( Read a, -- Weird
    Read b
  ) =>
  Read (a, b)

{- Weird before -}
class {- Weird0 -} Foo {- Weird1 -} ({- Weird2 -} a {- Weird3 -} :: {- Weird4 -} Type {- Weird5 -}) {- Weird6 -}
{- Weird after -}

instance Eq Foo where
  -- Weird
  Foo == Foo = True
