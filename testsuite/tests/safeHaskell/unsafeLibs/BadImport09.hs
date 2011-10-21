{-# LANGUAGE Safe #-}
-- | Import unsafe module Foreign.Marshal to make sure it fails
module Main where

import Foreign.Marshal

f :: Int
f = unsafeLocalState $ putStrLn "What kind of swallow?" >> return 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

