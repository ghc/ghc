{-# LANGUAGE UnboxedSums, MagicHash, BangPatterns, UnboxedTuples #-}

module Main where

import GHC.Prim
import GHC.Types

import Data.Void (Void)
import System.Mem (performMajorGC)

showAlt0 :: (# Void# | (# #) | () #) -> String
showAlt0 (# | (# #) | #) = "(# | (# #) | #)"
showAlt0 (# | | () #) = "(# | | () #)"

showAlt1 :: (# Void | Float# #) -> String
showAlt1 (# _ | #) = "(# Void | #)"
showAlt1 (# | f #) = "(# | " ++ show (F# f) ++ "# #)"

data D = D { f1 :: (# Void# | (# #) | () #)
           , f2 :: (# Void | Float# #)
           }

showD :: D -> String
showD (D f1 f2) = showAlt0 f1 ++ "\n" ++ showAlt1 f2

main :: IO ()
main = do
    putStrLn (showAlt0 (# | (# #) | #))
    putStrLn (showAlt0 (# | | () #))
    putStrLn (showAlt1 (# undefined | #))
    putStrLn (showAlt1 (# | 8.1# #))
    putStrLn (showD (D (# | (# #) | #) (# | 1.2# #)))
    performMajorGC
