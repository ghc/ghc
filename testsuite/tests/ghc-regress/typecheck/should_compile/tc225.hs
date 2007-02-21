{-# OPTIONS -fglasgow-exts #-}

-- Newtype in GADT syntax

module ShouldCompile where

newtype Bug a where Bug :: a -> Bug a
