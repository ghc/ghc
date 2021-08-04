{-# LANGUAGE OverloadedStrings #-}

module Warp (benchmarks) where

import Criterion.Main (bench, bgroup, nf)
import Criterion.Types (Benchmark)
import Data.ByteString (ByteString)
import Network.Wai.Handler.Warp.ReadInt (readInt)
import qualified Data.Attoparsec.ByteString.Char8 as B

benchmarks :: Benchmark
benchmarks = bgroup "warp" [
    bgroup "decimal" [
      bench "warp" $ nf (readInt :: ByteString -> Int) "31337"
    , bench "atto" $ nf (B.parse (B.decimal :: B.Parser Int)) "31337"
    ]
  ]
