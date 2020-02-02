{-# LANGUAGE RankNTypes, ViewPatterns #-}

module ViewPats where

ex3 :: forall a. a -> a -> Int -> Eq a => Bool
-- Reject: only skolemise over two args
--    const (result :: Bool) :: b -> Eq a => Bool
ex3 x ((== x) -> result) = const result
