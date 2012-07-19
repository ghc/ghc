{-# OPTIONS_HADDOCK prune #-}
module PruneWithWarning (foo, bar) where

foo :: Int
foo = 23
{-# DEPRECATED foo "use bar instead" #-}

bar :: Int
bar = 42
