{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module DirectIndexedListClient (firstCell) where

import Data.IORef (newIORef, readIORef)
import DirectIndexedList (IndexedList(IndexedList))

-- | Read the first cell after rebuilding the list at its original index.
firstCell :: IO [Bool]
firstCell = do
    xs <- newIORef (True : error "forced direct indexed list tail") >>= readIORef
    pure $ case rebuild (IndexedList xs) of
        IndexedList ys -> take 1 ys
  where
    rebuild :: IndexedList xs -> IndexedList xs
    rebuild (IndexedList ys) = IndexedList ys
