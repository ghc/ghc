{-# LANGUAGE Safe #-}
-- | Import unsafe module Debug.Trace to make sure it fails
module Main where

import Debug.Trace

f :: Int
f = trace "What kind of swallow?" 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

