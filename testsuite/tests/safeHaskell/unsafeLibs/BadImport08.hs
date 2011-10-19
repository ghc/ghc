{-# LANGUAGE Safe #-}
-- | Import unsafe module Control.ST to make sure it fails
module Main where

import Control.ST

f :: Int
f = trace "What kind of swallow?" 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

