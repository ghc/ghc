{-# OPTIONS -fglasgow-exts #-}

-- !!! Implicit parameters, and Rank-2 types
-- This one made the 5.00.1 not print anything

module Main where

foo :: ((?x :: Int) => IO a) -> Int -> IO a
	-- Note the rank2 type
foo s z = do  s with ?x = z
	      s with ?x = z+3

main = foo (print ?x) 42

