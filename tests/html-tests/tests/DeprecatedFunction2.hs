module DeprecatedFunction2 where


foo :: Int
foo = 23
{-# DEPRECATED foo "use bar instead" #-}
