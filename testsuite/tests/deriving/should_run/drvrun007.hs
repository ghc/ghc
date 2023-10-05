module Main( main ) where
-- This one crashed Hugs98

data X = X | X :\ X  deriving Show

main = putStrLn (show (X :\ X))
