{-# LANGUAGE MagicHash #-}
module Overflow where

import GHC.Exts

-- Overflow an 'Int#' expression
f x = let y :: Int#
          y = 10000000000000000000000000000000#
      in 9

-- Overflow an 'Int#' pattern
g :: Int# -> Bool
g 100000000000000000000000000# = True
g _ = False

-- Overflow an 'Int' expression
h :: Int
h = 1000000000000000000000000000000

-- Overflow an 'Int' pattern
i :: Int -> Int
i 100000000000000000000000000000000 = 0
i _ = 1

-- Underflow a 'Word' expression
j :: Word
j = -1

-- Underflow a 'Word' pattern
k :: Word -> Bool
k (-1) = True
k _ = False
