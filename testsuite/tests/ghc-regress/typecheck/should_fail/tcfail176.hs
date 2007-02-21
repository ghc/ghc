{-# OPTIONS -fglasgow-exts #-}

-- Newtype in GADT syntax

module ShouldFail where

newtype Bug a where Bug :: a -> Maybe a
