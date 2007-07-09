
{-# OPTIONS_GHC -XPolymorphicComponents #-}

module ShouldCompile where

newtype Swizzle = MkSwizzle (forall a. Ord a => [a] -> [a])

