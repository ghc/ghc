{-# LANGUAGE GHC2021 #-}

module T4532 where

-- This checks that GHC2021 is recognized, and indeed
-- enables stuff like BinaryLiterals and NumericUnderscores
foo = 0b1010_0101
