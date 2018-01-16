{-# LANGUAGE UnboxedSums, MagicHash, UnboxedTuples #-}

-- Nesting sums and tuples is OK

module Main where

import GHC.Prim
import GHC.Types

import System.Mem (performMajorGC)

type S_T_T a b c d = (# (# a , b #) | (# c , d #) #)
type S_S_S a b c d = (# (# a | b #) | (# c | d #) #)

show_stt :: (Show a, Show b, Show c, Show d) => S_T_T a b c d -> String
show_stt (# (# a, b #) | #) = show a ++ show b
show_stt (# | (# c, d #) #) = show c ++ show d

show_sss :: (Show a, Show b, Show c, Show d) => S_S_S a b c d -> String
show_sss (# (# a | #) | #) = show a
show_sss (# (# | b #) | #) = show b
show_sss (# | (# c | #) #) = show c
show_sss (# | (# | d #) #) = show d

main :: IO ()
main = do
    putStrLn (show_stt stt)
    putStrLn (show_sss sss)
    performMajorGC
  where
    stt :: S_T_T Int Bool Float String
    stt = (# (# 123, True #) | #)

    sss :: S_S_S Int Bool Float String
    sss = (# | (# 1.2 | #) #)
