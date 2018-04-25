{-# LANGUAGE Safe #-}
-- | Import SYB stuff that should be safe
module Main where

import Data.Typeable
import Data.Dynamic
import Data.Data

f :: Int
f = 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

