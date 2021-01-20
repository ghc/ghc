{-# LANGUAGE Haskell2010 #-}

module ShouldCompile where

newtype Swizzle = MkSwizzle (forall a. Ord a => [a] -> [a])
