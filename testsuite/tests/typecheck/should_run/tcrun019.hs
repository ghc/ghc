{-# LANGUAGE ImplicitParams, RankNTypes #-}

-- GHC 5.02 got this one wrong.

module Main where

-- Implicit parameters bug
-- mbs@cse.ogi.edu 24-Oct-2001 22:21:27

f :: (?x :: Int) => ((?x :: Int) => Int) -> Int -> Int
f g y = if y == 0 
	then g 
	else let ?x = ?x + 1 
	     in f g (y - 1)

h :: (?x :: Int) => Int
h = ?x

main = print (let ?x = 0 in f h 10)
-- The result should be 10!

