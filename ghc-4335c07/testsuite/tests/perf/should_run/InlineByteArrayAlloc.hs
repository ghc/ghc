{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = loop 10000000
  where
    loop :: Int -> IO ()
    loop 0 = return ()
    loop i = newByteArray >> loop (i-1)

newByteArray :: IO ()
newByteArray = IO $ \s -> case newByteArray# 128# s of
    (# s', _ #) -> (# s', () #)
