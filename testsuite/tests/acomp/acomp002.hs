{-# LANGUAGE ApplicativeComprehensions #-}
module Test where

-- Test that type errors aren't affected by ApplicativeDo
f :: IO (Int,Int)
f = [(y, x) | x <- getChar, y <- getChar]

g :: IO (Int,Int)
g = [(x2, x4) | x1 <- getChar, x2 <- getChar, x3 <- const (pure ()) x1, x4 <- getChar, x5 <- getChar x4]
