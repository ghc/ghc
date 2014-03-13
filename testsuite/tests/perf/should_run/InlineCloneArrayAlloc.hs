{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    marr <- newArray
    loop 10000000 (unMArray marr)
  where
    loop :: Int -> MutableArray# RealWorld () -> IO ()
    loop 0 _    = return ()
    loop i marr = freezeArray marr >> loop (i-1) marr

data MArray = MArray { unMArray :: !(MutableArray# RealWorld ()) }

newArray :: IO MArray
newArray = IO $ \s -> case newArray# 16# () s of
    (# s', marr# #) -> (# s', MArray marr# #)

freezeArray :: MutableArray# RealWorld () -> IO ()
freezeArray marr# = IO $ \s -> case freezeArray# marr# 0# 16# s of
    (# s', _ #) -> (# s', () #)
