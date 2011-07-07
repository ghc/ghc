{-# LANGUAGE Safe #-}
module Main where

import System.IO.Unsafe

f :: Int
f = unsafePerformIO $ putStrLn "What kind of swallow?" >> return 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

