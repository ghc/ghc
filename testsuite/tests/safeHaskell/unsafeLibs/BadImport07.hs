{-# LANGUAGE Safe #-}
-- | Import unsafe module Unsafe.Coerce to make sure it fails
module Main where

import Unsafe.Coerce

f :: Int
f = trace "What kind of swallow?" 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

