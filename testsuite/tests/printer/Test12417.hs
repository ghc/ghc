{-# LANGUAGE UnboxedSums, MagicHash #-}

module Test12417 where

import GHC.Prim
import GHC.Types

import System.Mem (performMajorGC)

type Either1 a b = (# a | b #)

showEither1 :: (Show a, Show b) => Either1 a b -> String
showEither1 (# left | #)  = "Left " ++ show left
showEither1 (# | right #) = "Right " ++ show right

type T = (# Int | Bool | String | Char | Either Int Bool | Int# | Float# #)

showEither4 :: T -> String
showEither4 (# | b | | | | | #) = "Alt1: " ++ show b
