{-# LANGUAGE ApplicativeComprehensions #-}
module ShouldFail where

g :: IO ()
g = [() | x <- getChar, 'a' <- pure (3 :: Int) {- type error -}]
