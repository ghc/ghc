{-# OPTIONS -fglasgow-exts #-}

-- GHC 5.02 got this one wrong.

module Main where

-- Implicit parameters bug
-- mbs@cse.ogi.edu 24-Oct-2001 22:21:27

f :: (?x :: Int) => ((?x :: Int) => Int) -> Int -> Int
f g y = if y == 0 
	then g 
	else (f g (y - 1) with ?x = ?x + 1)

h :: (?x :: Int) => Int
h = ?x

main = print (f h 10 with ?x = 0)
-- The result should be 10!

