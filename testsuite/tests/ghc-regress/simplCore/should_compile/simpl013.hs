{-# OPTIONS -fglasgow-exts #-}

-- This one made GHC 6.4.1 panic because of over-zealous 
-- complaining in mkCase1 when there was an empty list of
-- alternatives

module Foo2 where

data FooBar  = Foo | Bar

data P a = P0 | P1 FooBar

data PS p where
    C :: P Int -> p b -> PS p

f :: PS P -> Char
f (C (P1 _)   P0)       = 'a'
f (C (P1 Bar) (P1 Bar)) = 'b'
f (C (P1 _)   (P1 Bar)) = 'c'
