{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module DirectIndexedList (IndexedList(IndexedList), Reading(Reading)) where

-- | Store a runtime list at its own index.
data IndexedList (xs :: [Bool]) where
    IndexedList :: foreach (ys :: [Bool]) -> IndexedList ys

-- | Record whether the instrument was calibrated when it took the reading.
data Reading (calibrated :: Bool) where
    Reading :: foreach (b :: Bool) -> Int -> Reading b
