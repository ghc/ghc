{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module DirectIndexedList (IndexedList(IndexedList)) where

-- | Store a runtime list at its own index.
data IndexedList (xs :: [Bool]) where
    IndexedList :: foreach (ys :: [Bool]) -> IndexedList ys
