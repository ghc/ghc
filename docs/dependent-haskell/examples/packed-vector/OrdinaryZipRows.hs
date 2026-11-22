{-# LANGUAGE NoExperimentalDependentTypes #-}

module OrdinaryZipRows (headerCountReport, zipReport) where

import PackedVecLibrary (natToInt, toList)
import ZipLengthSource (countWithHeader)
import ZipRows (countWithHeaderStep, zipMinimum, zipRows)

-- | Show the paired rows with the proof of their length.
zipReport :: (Show a, Show b) => [a] -> [b] -> String
zipReport xs ys = show (toList (zipRows xs ys), zipMinimum xs ys)

-- | Show the counts with and without a header, and the one-record proof.
headerCountReport :: [Bool] -> String
headerCountReport records = show
    ( natToInt (countWithHeader ["header"] records)
    , natToInt (countWithHeader ([] :: [String]) (undefined :: [Bool]))
    , countWithHeaderStep "header" [] True records
    )
