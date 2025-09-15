{-# LANGUAGE UnboxedSums #-}

module Lib where

-- No spaces needed in the type syntax
type T = (#Int|Bool|String#)

-- Term syntax needs spaces, otherwise we parser bars as sections
-- for ||, ||| etc.
--
-- t1 :: T
-- t1 = (# 10 | | #)
