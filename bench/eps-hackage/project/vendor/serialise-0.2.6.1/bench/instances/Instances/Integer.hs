{-# LANGUAGE ScopedTypeVariables #-}

module Instances.Integer
  ( benchmarks -- :: [Benchmark]
  ) where

import           Criterion.Main
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Vector            as Vector

import           Codec.Serialise

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "serialise"
    [ bench "small positive" (nf goSerialise integerDataSmallPos)
    , bench "small negative" (nf goSerialise integerDataSmallNeg)
    , bench "large positive" (nf goSerialise integerDataLargePos)
    , bench "large negative" (nf goSerialise integerDataLargeNeg)
    ]
  , bgroup "deserialise"
    [ bench "small positive" (nf goDeserialise integerDataSerialisedSmallPos)
    , bench "small negative" (nf goDeserialise integerDataSerialisedSmallNeg)
    , bench "large positive" (nf goDeserialise integerDataSerialisedLargePos)
    , bench "large negative" (nf goDeserialise integerDataSerialisedLargeNeg)
    ]
  ]
  where
    goSerialise = BS.length . serialise
    goDeserialise :: BS.ByteString -> Vector.Vector Integer
    goDeserialise = deserialise
    integerDataSmallPos = Vector.replicate (100 :: Int) (10 :: Integer)
    integerDataSmallNeg = Vector.replicate (100 :: Int) (-10 :: Integer)
    integerDataLargePos = Vector.replicate (100 :: Int) (two^(two * 100))
    integerDataLargeNeg = Vector.replicate (100 :: Int) (-(two^(two * 100)))
    integerDataSerialisedSmallPos = serialise integerDataSmallPos
    integerDataSerialisedSmallNeg = serialise integerDataSmallNeg
    integerDataSerialisedLargePos = serialise integerDataLargePos
    integerDataSerialisedLargeNeg = serialise integerDataLargeNeg
    two = 2 :: Integer

