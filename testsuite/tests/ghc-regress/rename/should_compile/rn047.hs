{-# OPTIONS -fglasgow-exts -fwarn-unused-matches #-}
-- Tests unused-variable warnings in parallel list comprehensions

module ShouldCompile where

t :: [Int]
t = [ q | y <- [1..10] 
        | z <- [30..40], let q = z*z]
