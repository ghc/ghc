{-# OPTIONS -fglasgow-exts #-}

-- !!! Implicit parameter test

module Main where

main = do { putStrLn $ show $ foo with ?x = 13 
	  ; putStrLn $ show $ baz () with ?x = 14 }

foo :: (?x :: Int) => Int
foo = ?x

-- Check that defaulting works too
baz () = ?x

