{-# LANGUAGE Safe #-}
-- | Import unsafe module System.IO.Unsafe to make sure it fails
module Main where

import System.IO.Unsafe

f :: Int
f = unsafePerformIO $ putStrLn "What kind of swallow?" >> return 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

