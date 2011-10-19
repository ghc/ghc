{-# LANGUAGE Safe #-}
-- | Import unsafe module Foreign to make sure it fails
module Main where

import Foreign (unsafePerformIO)

f :: Int
f = unsafePerformIO $ putStrLn "What kind of swallow?" >> return 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

