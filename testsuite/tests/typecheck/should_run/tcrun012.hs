{-# LANGUAGE ImplicitParams #-}

-- !!! Implicit parameter test

module Main where

main = do { let ?x = 13 in putStrLn $ show $ foo
	  ; let ?x = 14 in putStrLn $ show $ baz () }

foo :: (?x :: Int) => Int
foo = ?x

-- Check that defaulting works too
baz () = ?x

