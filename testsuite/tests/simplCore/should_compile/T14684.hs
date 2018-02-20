-- This is a test for the combine-identical-alternatives optimisation.
-- The alternatives with the most common RHS are combined into
-- a single DEFAULT alternative.


module T14684 where

data Foo = Foo1 | Foo2 | Foo3 !Int | Foo4 | Foo5 | Foo6

fun1 :: Foo -> Int
{-# NOINLINE fun1 #-}
fun1 x = case x of
               Foo1 -> 0
               Foo2 -> 1
               Foo3 {} -> 2
               Foo4 -> 1
               Foo5 -> 2
               Foo6 -> 2
