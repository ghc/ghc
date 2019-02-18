module DeprecatedFunction where

-- | some documentation for foo
foo :: Int
foo = 23
{-# DEPRECATED foo "use `bar` instead" #-}

-- | some documentation for bar
bar :: Int
bar = 42
