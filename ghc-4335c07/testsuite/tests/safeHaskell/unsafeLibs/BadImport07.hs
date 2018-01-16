{-# LANGUAGE Safe #-}
-- | Import unsafe module Unsafe.Coerce to make sure it fails
module Main where

import Unsafe.Coerce

f :: Int
f = 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

