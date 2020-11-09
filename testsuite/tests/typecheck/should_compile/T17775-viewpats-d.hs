{-# LANGUAGE RankNTypes, ViewPatterns #-}

module ViewPats where

ex6 :: forall a. a -> a -> Int -> Eq a => Bool
-- Reject. Needs (const (bla :: Bool)) :: Int -> Eq a => Bool
ex6 x y = const (x == y)
