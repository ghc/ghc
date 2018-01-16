
{-# OPTIONS_GHC -XScopedTypeVariables #-}
-- We don't actually want scoped type variables, but this flag makes the
-- forall be recognised by the parser

module ShouldCompile where

newtype Swizzle = MkSwizzle (forall a. Ord a => [a] -> [a])
