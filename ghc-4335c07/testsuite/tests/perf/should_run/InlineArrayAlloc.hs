{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = loop 10000000
  where
    loop :: Int -> IO ()
    loop 0 = return ()
    loop i = newArray >> loop (i-1)

newArray :: IO ()
newArray = IO $ \s -> case newArray# 16# () s of
    (# s', _ #) -> (# s', () #)
