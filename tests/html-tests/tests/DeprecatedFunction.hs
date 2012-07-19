module DeprecatedFunction where

-- | some documentation foo
foo :: Int
foo = 23
{-# DEPRECATED foo "use bar instead" #-}
