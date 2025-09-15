
{-# LANGUAGE ImplicitParams, ImpredicativeTypes #-}

module T20654a where

foo :: (?poly :: forall a. a -> a) => Int -> Int
foo x = ?poly x
