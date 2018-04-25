{-# LANGUAGE ImplicitParams, RankNTypes #-}

-- Rank-2 types with implicit parameters.
-- GHC 5.02 erroneously rejected this

module ShouldCompile where

    f :: ((?param :: a) => b) -> a -> b
    f foo a = let ?param = a in foo 

    g :: (?param :: a) => a
    g = ?param

    h :: a -> a
    h = f g
