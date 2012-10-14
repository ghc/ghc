{-# OPTIONS_HADDOCK prune #-}
-- |
-- What is tested here:
--
-- * If a binding has a deprecation message but no documentation, it is pruned
--   when @OPTIONS_HADDOCK prune@ is used.
--
module PruneWithWarning (foo, bar) where

foo :: Int
foo = 23
{-# DEPRECATED foo "use bar instead" #-}

bar :: Int
bar = 42
