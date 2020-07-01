{-# LANGUAGE NoNegativeLiterals, MagicHash, BinaryLiterals #-}

-- Even when NegativeLiterals are disabled,
-- we parse unboxed literals appropriately.
module NegativeLiteralsNoExt where

import GHC.Exts

------------------------------------
-- Prefix occurrence of the minus --
------------------------------------

p2 :: Int
p2 = I# -1#      -- parsed as:  I# (-1#)

p4 :: Float
p4 = F# -0.01#   -- parsed as:  F# (-0.01#)

p5 :: Double
p5 = D# -0.01##  -- parsed as:  D# (-0.01##)

-----------------------------------------
-- Tight infix occurrence of the minus --
-----------------------------------------

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
