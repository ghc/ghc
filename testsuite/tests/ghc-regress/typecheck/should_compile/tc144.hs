{-# OPTIONS -fglasgow-exts #-}

-- Rank-2 types with implicit parameters.
-- GHC 5.02 erroneously rejected this

module ShouldCompile where

    f :: ((?param :: a) => b) -> a -> b
    f foo a = foo with ?param=a

    g :: (?param :: a) => a
    g = ?param

    h :: a -> a
    h = f g
