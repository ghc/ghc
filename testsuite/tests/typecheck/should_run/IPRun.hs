{-# LANGUAGE ImplicitParams, RankNTypes #-}

module Main where

f0 :: (?x :: Int) => () -> Int
f0 () = let ?x = 5 in ?x
   -- Should always return 5

f1 :: (?x :: Int) => () -> Int
f1 = let ?x = 5 in \() -> ?x
   -- Should always return 5

f2 () = let ?x = 5 in \() -> ?x
   -- Inferred type: Num a => () -> () -> a
   -- should always return 5

f3 :: () -> ((?x :: Int) => Int)
-- Simple subsumption means that the x=0 from main wins
f3 = let ?x = 5 in \() -> ?x

main = let ?x = 0 in
       do { print (f0 ())
          ; print (f1 ())
          ; print (f2 () ())
          ; print (f3 ()) }

