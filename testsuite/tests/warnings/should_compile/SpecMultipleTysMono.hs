module SpecMultipleTysMono where

-- NB: this program should be rejected starting from GHC 9.18.
-- See GHC ticket #25540.

foo :: Bool -> Bool
foo = not

{-# SPECIALISE foo :: Bool -> Bool, Bool -> Bool #-}
