-- Don't warn about specialization failures for class ops.

{-# OPTIONS_GHC -O -Wall-missed-specialisations #-}
module T19586 where

type MyConstraint a b = (Show a, Enum b, Show b)

foo :: MyConstraint a b => Int -> a -> b -> (String, String)
foo 0 x y = (show x, show . succ $ y)
foo n x y = foo (n-1) x y


bar :: Int -> Char -> (String, String)
bar x y = foo x x y
