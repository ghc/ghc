{-# LANGUAGE Strict #-}  -- NB: Strict
module T28137 where

import Data.Word

f :: Word8 -> Int
f 300 = 1   -- Should trigger a warning
f _   = 2
