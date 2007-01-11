{-# OPTIONS -fglasgow-exts -O2 #-}

-- Reading the interface file caused a black hole
-- in earlier versions of GHC

-- Also, foo should compile to very tight code with -O2
-- (The O2 was nothing to do with the black hole though.)

module ShouldCompile where

import ATLoop_help

foo :: FooT Int -> Int -> Int
foo t n = t `seq` bar n
   where
     bar 0 = 0
     bar n | even n = bar (n `div` 2)
     bar n          = bar (n - int t)




