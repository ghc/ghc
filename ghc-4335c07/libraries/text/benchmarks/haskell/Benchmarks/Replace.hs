{-# LANGUAGE BangPatterns #-}
-- | Replace a string by another string
--
-- Tested in this benchmark:
--
-- * Search and replace of a pattern in a text
--
module Benchmarks.Replace
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, nf)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Search as BL
import qualified Data.ByteString.Search as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL

benchmark :: FilePath -> String -> String -> IO Benchmark
benchmark fp pat sub = do
    tl <- TL.readFile fp
    bl <- BL.readFile fp
    let !t = TL.toStrict tl
        !b = T.encodeUtf8 t
    return $ bgroup "Replace" [
          bench "Text"           $ nf (T.length . T.replace tpat tsub) t
        , bench "ByteString"     $ nf (BL.length . B.replace bpat bsub) b
        , bench "LazyText"       $ nf (TL.length . TL.replace tlpat tlsub) tl
        , bench "LazyByteString" $ nf (BL.length . BL.replace blpat blsub) bl
        ]
  where
    tpat  = T.pack pat
    tsub  = T.pack sub
    tlpat = TL.pack pat
    tlsub = TL.pack sub
    bpat = T.encodeUtf8 tpat
    bsub = T.encodeUtf8 tsub
    blpat = B.concat $ BL.toChunks $ TL.encodeUtf8 tlpat
    blsub = B.concat $ BL.toChunks $ TL.encodeUtf8 tlsub
