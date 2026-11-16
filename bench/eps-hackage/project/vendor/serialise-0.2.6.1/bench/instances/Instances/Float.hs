{-# LANGUAGE ScopedTypeVariables #-}

module Instances.Float
  ( benchmarks -- :: [Benchmark]
  ) where

import           Criterion.Main
import           Codec.Serialise
import           Control.DeepSeq (force)
import qualified Data.ByteString.Lazy as BSL

benchmarks :: [Benchmark]
benchmarks =
  [ bench "serialise Float" (whnf (BSL.length . serialise) fakesF)
  , bench "deserialise Float" (nf (deserialise :: BSL.ByteString -> [Float]) serialF)
  , bench "serialise Double" (whnf (BSL.length . serialise) fakesD)
  , bench "deserialise Double" (nf (deserialise :: BSL.ByteString -> [Double]) serialD)
  ]
  where
    fakesF = force (replicate 100 (3.14159 :: Float))
    fakesD = force (replicate 100 (3.14159 :: Double))

    serialF = force (serialise fakesF)
    serialD = force (serialise fakesD)

