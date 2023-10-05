{-# LANGUAGE NegativeLiterals, MagicHash, BinaryLiterals #-}

module NegativeLiterals where

import GHC.Exts

------------------------------------
-- Prefix occurrence of the minus --
------------------------------------

p1 :: Bool
p1 = even -2     -- parsed as:  even (-2)

p2 :: Int
p2 = I# -1#      -- parsed as:  I# (-1#)

p3 :: Int
p3 = floor -2.4  -- parsed as:  floor (-2.4)

p4 :: Float
p4 = F# -0.01#   -- parsed as:  F# (-0.01#)

p5 :: Double
p5 = D# -0.01##  -- parsed as:  D# (-0.01##)

p6 :: Bool
p6 =   even -0b10  -- parsed as: even (-2)
    || even -0o10  -- parsed as: even (-8)
    || even -0x10  -- parsed as: even (-16)

-----------------------------------------
-- Tight infix occurrence of the minus --
-----------------------------------------

ti1 :: Integer -> Integer
ti1 x = x-2       -- parsed as:  (-) x 1

ti2 :: Int# -> Int#
ti2 x = x-1#      -- parsed as:  (-) x 1#
  where (-) = (-#)

ti3 :: Double -> Double
ti3 x = x-2.4     -- parsed as:  (-) x 2.4

ti4 :: Float# -> Float#
ti4 x = x-0.1#    -- parsed as:  (-) x 0.1#
  where (-) = minusFloat#

ti5 :: Double# -> Double#
ti5 x = x-0.1##   -- parsed as:  (-) x 0.1##
  where (-) = (-##)

ti6 :: Integer -> [Integer]
ti6 x =
  [ x-0b10,    -- parsed as: (-) x 2
    x-0o10,    -- parsed as: (-) x 8
    x-0x10  ]  -- parsed as: (-) x 16
