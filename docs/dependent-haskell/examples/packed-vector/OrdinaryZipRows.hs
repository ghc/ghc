{-# LANGUAGE NoExperimentalDependentTypes #-}

module OrdinaryZipRows (zipReport) where

import PackedVecLibrary (toList)
import ZipRows (zipMinimum, zipRows)

-- | Show the paired rows with the proof of their length.
zipReport :: (Show a, Show b) => [a] -> [b] -> String
zipReport xs ys = show (toList (zipRows xs ys), zipMinimum xs ys)
