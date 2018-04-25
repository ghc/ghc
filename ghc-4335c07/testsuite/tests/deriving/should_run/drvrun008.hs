-- !!! Check that -funbox-strict-fields doesn't mess up deriving (can't be in an options pragma, it's in the Makefile)
-- !!!  (it did in 4.04)

module Main( main ) where

data X = X !Int deriving Eq

main = putStrLn (show (X 2 == X 2))
