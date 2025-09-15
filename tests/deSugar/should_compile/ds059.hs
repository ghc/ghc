{-# OPTIONS -fwarn-incomplete-patterns #-}

-- Test for incomplete-pattern warnings
-- None should cause a warning

module ShouldCompile where

-- These ones gave bogus warnings in 6.2

data D = D1 { f1 :: Int } | D2

-- Use pattern matching in the argument
f :: D -> D
f  d1@(D1 {f1 = n}) = d1 { f1 = f1 d1 + n } -- Warning here
f  d = d

-- Use case pattern matching
g :: D -> D
g  d1 = case d1 of
           D1 { f1 = n } -> d1 { f1 = n + 1 } -- Warning here also
           D2            -> d1

-- These ones were from Neil Mitchell
-- no warning
ex1 x = ss
    where (_s:ss) = x

-- no warning
ex2 x = let (_s:ss) = x in ss

--    Warning: Pattern match(es) are non-exhaustive
--             In a case alternative: Patterns not matched: []
ex3 x = case x of ~(_s:ss) -> ss
