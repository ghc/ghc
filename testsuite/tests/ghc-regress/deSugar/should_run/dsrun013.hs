{-# OPTIONS_GHC -fglasgow-exts #-}

-- This one killed GHC 6.4.1, because the pattern match on the
-- unboxed tuple generates a failure case, which defeated the
-- rather fragile code in the desugarer
-- See DsExpr.lhs, the HsCase case

module Main where

foo xs ys = case (# null xs, null ys #) of
              (# True, False #) -> "One"
              (# False, True #) -> "Two"

main :: IO ()
main = print (foo [] "ok")

