-- | Testing the internal builder monoid
--
-- Tested in this benchmark:
--
-- * Concatenating many small strings using a builder
--
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Builder
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, nf)
import Data.Binary.Builder as B
import Data.ByteString.Char8 ()
import Data.Monoid (mconcat, mempty)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as Int
import Data.Int (Int64)

benchmark :: IO Benchmark
benchmark = return $ bgroup "Builder"
    [ bgroup "Comparison"
      [ bench "LazyText" $ nf
          (LT.length . LTB.toLazyText . mconcat . map LTB.fromText) texts
      , bench "Binary" $ nf
          (LB.length . B.toLazyByteString . mconcat . map B.fromByteString)
          byteStrings
      , bench "Blaze" $ nf
          (LB.length . Blaze.toLazyByteString . mconcat . map Blaze.fromString)
          strings
      ]
    , bgroup "Int"
      [ bgroup "Decimal"
        [ bgroup "Positive" .
          flip map numbers $ \n ->
          (bench (show (length (show n))) $ nf (LTB.toLazyText . Int.decimal) n)
        , bgroup "Negative" .
          flip map numbers $ \m ->
          let n = negate m in
          (bench (show (length (show n))) $ nf (LTB.toLazyText . Int.decimal) n)
        , bench "Empty" $ nf LTB.toLazyText mempty
        , bgroup "Show" .
          flip map numbers $ \n ->
          (bench (show (length (show n))) $ nf show n)
        ]
      ]
    ]
  where
    numbers :: [Int64]
    numbers = [
      6, 14, 500, 9688, 10654, 620735, 5608880, 37010612,
      731223504, 5061580596, 24596952933, 711732309084, 2845910093839,
      54601756118340, 735159434806159, 3619097625502435, 95777227510267124,
      414944309510675693, 8986407456998704019
     ]

texts :: [T.Text]
texts = take 200000 $ cycle ["foo", "λx", "由の"]
{-# NOINLINE texts #-}

-- Note that the non-ascii characters will be chopped
byteStrings :: [SB.ByteString]
byteStrings = take 200000 $ cycle ["foo", "λx", "由の"]
{-# NOINLINE byteStrings #-}

-- Note that the non-ascii characters will be chopped
strings :: [String]
strings = take 200000 $ cycle ["foo", "λx", "由の"]
{-# NOINLINE strings #-}
