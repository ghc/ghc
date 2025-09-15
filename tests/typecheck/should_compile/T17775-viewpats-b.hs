{-# LANGUAGE RankNTypes, ViewPatterns #-}

module ViewPats where

ex2 :: forall a. a -> a -> Int -> Eq a => Bool
-- Reject: only skolemise over two args
ex2 x ((== x) -> result) = \ _ -> result
