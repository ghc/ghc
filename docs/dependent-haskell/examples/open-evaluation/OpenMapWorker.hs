{-# LANGUAGE NoExperimentalDependentTypes #-}
module OpenMapWorker (each) where

-- | Apply a function to each element and keep the result fields lazy.
each :: (a -> b) -> [a] -> [b]
each _ [] = []
each f (x:xs) = f x : each f xs
