{-# LANGUAGE Safe #-}
-- | Import unsafe module Control.ST to make sure it fails
module Main where

import Control.Monad.ST

f :: Int
f = 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

