{-# LANGUAGE ScopedTypeVariables #-}

module Instances.Time
  ( benchmarks -- :: [Benchmark]
  ) where

import           Criterion.Main
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Calendar (Day(..))
import           Codec.Serialise
import           Control.DeepSeq (force)
import qualified Data.ByteString.Lazy as BSL

benchmarks :: [Benchmark]
benchmarks =
  [ bench "serialise UTCTime" (whnf (BSL.length . serialise) timestamps)
  , bench "deserialise UTCTime" (nf (deserialise :: BSL.ByteString -> [UTCTime]) serialisedTimestamps)
  ]
  where
    faketime             = UTCTime (ModifiedJulianDay 0) 0
    timestamps           = force (replicate 100 faketime)
    serialisedTimestamps = force (serialise timestamps)
