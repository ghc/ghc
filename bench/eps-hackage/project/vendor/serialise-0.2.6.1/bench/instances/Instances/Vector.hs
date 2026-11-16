{-# LANGUAGE ScopedTypeVariables #-}

module Instances.Vector
  ( benchmarks -- :: [Benchmark]
  ) where

import           Data.Proxy
import           Criterion.Main
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL
import           Codec.Serialise
import           Control.DeepSeq (force)

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "unboxed"
      [ bgroup "encode"
          [ vectorEncode unboxed (42 :: Int) 500
          , vectorEncode unboxed (42 :: Int) 5000
          , vectorEncode unboxed (42 :: Int) 50000
          , vectorEncode unboxed (42 :: Int) 500000
          ]
      , bgroup "decode"
          [ vectorDecode unboxed (42 :: Int) 500
          , vectorDecode unboxed (42 :: Int) 5000
          , vectorDecode unboxed (42 :: Int) 50000
          , vectorDecode unboxed (42 :: Int) 500000
          ]
      ]
  , bgroup "boxed"
      [ bgroup "encode"
          [ vectorEncode boxed (42 :: Int) 500
          , vectorEncode boxed (42 :: Int) 5000
          , vectorEncode boxed (42 :: Int) 50000
          , vectorEncode boxed (42 :: Int) 500000
          ]
      , bgroup "decode"
          [ vectorDecode boxed (42 :: Int) 500
          , vectorDecode boxed (42 :: Int) 5000
          , vectorDecode boxed (42 :: Int) 50000
          , vectorDecode boxed (42 :: Int) 500000
          ]
      ]
  ]
  where
    unboxed = Proxy :: Proxy VU.Vector
    boxed   = Proxy :: Proxy V.Vector

--------------------------------------------------------------------------------
-- Encoding

vectorEncode :: forall v a. (Serialise (v a), VG.Vector v a)
  => Proxy v
  -> a
  -> Int
  -> Benchmark
vectorEncode Proxy x len
  = bench (show len) $ benchEncode (asVector $ VG.replicate len x)
  where
    asVector v = v :: v a

    benchEncode :: Serialise s => s -> Benchmarkable
    benchEncode = whnf (BSL.length . serialise)
    {-# INLINE benchEncode #-}

--------------------------------------------------------------------------------
-- Decoding

vectorDecode :: forall v a. (Serialise (v a), VG.Vector v a, Num a)
  => Proxy v
  -> a
  -> Int
  -> Benchmark
vectorDecode Proxy x len
  = bench (show len) $ benchDecode (force $ serialise $ asVector $ VG.replicate len x)
  where
    asVector v = v :: v a

    benchDecode :: BSL.ByteString -> Benchmarkable
    benchDecode = whnf (VG.sum . asVector . deserialise)
    {-# INLINE benchDecode #-}
