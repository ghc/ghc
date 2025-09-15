-- This one caused a panic in GHC 6.4

module Foo1 where

data List elem = Cons elem List | Nil

t1 :: List
t1 = Cons 1 Nil
