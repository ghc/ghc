-- | Benchmarks various pure functions from the Text library
--
-- Tested in this benchmark:
--
-- * Most pure functions defined the string types
--
{-# LANGUAGE BangPatterns, CPP, GADTs, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Benchmarks.Pure
    ( benchmark
    ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Criterion (Benchmark, bgroup, bench, nf)
import Data.Char (toLower, toUpper)
import Data.Monoid (mappend, mempty)
import GHC.Base (Char (..), Int (..), chr#, ord#, (+#))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as TL

benchmark :: String -> FilePath -> IO Benchmark
benchmark kind fp = do
    -- Evaluate stuff before actually running the benchmark, we don't want to
    -- count it here.

    -- ByteString A
    bsa     <- BS.readFile fp

    -- Text A/B, LazyText A/B
    ta      <- evaluate $ T.decodeUtf8 bsa
    tb      <- evaluate $ T.toUpper ta
    tla     <- evaluate $ TL.fromChunks (T.chunksOf 16376 ta)
    tlb     <- evaluate $ TL.fromChunks (T.chunksOf 16376 tb)

    -- ByteString B, LazyByteString A/B
    bsb     <- evaluate $ T.encodeUtf8 tb
    bla     <- evaluate $ BL.fromChunks (chunksOf 16376 bsa)
    blb     <- evaluate $ BL.fromChunks (chunksOf 16376 bsb)

    -- String A/B
    sa      <- evaluate $ UTF8.toString bsa
    sb      <- evaluate $ T.unpack tb

    -- Lengths
    bsa_len <- evaluate $ BS.length bsa
    ta_len  <- evaluate $ T.length ta
    bla_len <- evaluate $ BL.length bla
    tla_len <- evaluate $ TL.length tla
    sa_len  <- evaluate $ L.length sa

    -- Lines
    bsl     <- evaluate $ BS.lines bsa
    bll     <- evaluate $ BL.lines bla
    tl      <- evaluate $ T.lines ta
    tll     <- evaluate $ TL.lines tla
    sl      <- evaluate $ L.lines sa

    return $ bgroup "Pure"
        [ bgroup "append"
            [ benchT   $ nf (T.append tb) ta
            , benchTL  $ nf (TL.append tlb) tla
            , benchBS  $ nf (BS.append bsb) bsa
            , benchBSL $ nf (BL.append blb) bla
            , benchS   $ nf ((++) sb) sa
            ]
        , bgroup "concat"
            [ benchT   $ nf T.concat tl
            , benchTL  $ nf TL.concat tll
            , benchBS  $ nf BS.concat bsl
            , benchBSL $ nf BL.concat bll
            , benchS   $ nf L.concat sl
            ]
        , bgroup "cons"
            [ benchT   $ nf (T.cons c) ta
            , benchTL  $ nf (TL.cons c) tla
            , benchBS  $ nf (BS.cons c) bsa
            , benchBSL $ nf (BL.cons c) bla
            , benchS   $ nf (c:) sa
            ]
        , bgroup "concatMap"
            [ benchT   $ nf (T.concatMap (T.replicate 3 . T.singleton)) ta
            , benchTL  $ nf (TL.concatMap (TL.replicate 3 . TL.singleton)) tla
            , benchBS  $ nf (BS.concatMap (BS.replicate 3)) bsa
            , benchBSL $ nf (BL.concatMap (BL.replicate 3)) bla
            , benchS   $ nf (L.concatMap (L.replicate 3 . (:[]))) sa
            ]
        , bgroup "decode"
            [ benchT   $ nf T.decodeUtf8 bsa
            , benchTL  $ nf TL.decodeUtf8 bla
            , benchBS  $ nf BS.unpack bsa
            , benchBSL $ nf BL.unpack bla
            , benchS   $ nf UTF8.toString bsa
            ]
        , bgroup "decode'"
            [ benchT   $ nf T.decodeUtf8' bsa
            , benchTL  $ nf TL.decodeUtf8' bla
            ]
        , bgroup "drop"
            [ benchT   $ nf (T.drop (ta_len `div` 3)) ta
            , benchTL  $ nf (TL.drop (tla_len `div` 3)) tla
            , benchBS  $ nf (BS.drop (bsa_len `div` 3)) bsa
            , benchBSL $ nf (BL.drop (bla_len `div` 3)) bla
            , benchS   $ nf (L.drop (sa_len `div` 3)) sa
            ]
        , bgroup "encode"
            [ benchT   $ nf T.encodeUtf8 ta
            , benchTL  $ nf TL.encodeUtf8 tla
            , benchBS  $ nf BS.pack sa
            , benchBSL $ nf BL.pack sa
            , benchS   $ nf UTF8.fromString sa
            ]
        , bgroup "filter"
            [ benchT   $ nf (T.filter p0) ta
            , benchTL  $ nf (TL.filter p0) tla
            , benchBS  $ nf (BS.filter p0) bsa
            , benchBSL $ nf (BL.filter p0) bla
            , benchS   $ nf (L.filter p0) sa
            ]
        , bgroup "filter.filter"
            [ benchT   $ nf (T.filter p1 . T.filter p0) ta
            , benchTL  $ nf (TL.filter p1 . TL.filter p0) tla
            , benchBS  $ nf (BS.filter p1 . BS.filter p0) bsa
            , benchBSL $ nf (BL.filter p1 . BL.filter p0) bla
            , benchS   $ nf (L.filter p1 . L.filter p0) sa
            ]
        , bgroup "foldl'"
            [ benchT   $ nf (T.foldl' len 0) ta
            , benchTL  $ nf (TL.foldl' len 0) tla
            , benchBS  $ nf (BS.foldl' len 0) bsa
            , benchBSL $ nf (BL.foldl' len 0) bla
            , benchS   $ nf (L.foldl' len 0) sa
            ]
        , bgroup "foldr"
            [ benchT   $ nf (L.length . T.foldr (:) []) ta
            , benchTL  $ nf (L.length . TL.foldr (:) []) tla
            , benchBS  $ nf (L.length . BS.foldr (:) []) bsa
            , benchBSL $ nf (L.length . BL.foldr (:) []) bla
            , benchS   $ nf (L.length . L.foldr (:) []) sa
            ]
        , bgroup "head"
            [ benchT   $ nf T.head ta
            , benchTL  $ nf TL.head tla
            , benchBS  $ nf BS.head bsa
            , benchBSL $ nf BL.head bla
            , benchS   $ nf L.head sa
            ]
        , bgroup "init"
            [ benchT   $ nf T.init ta
            , benchTL  $ nf TL.init tla
            , benchBS  $ nf BS.init bsa
            , benchBSL $ nf BL.init bla
            , benchS   $ nf L.init sa
            ]
        , bgroup "intercalate"
            [ benchT   $ nf (T.intercalate tsw) tl
            , benchTL  $ nf (TL.intercalate tlw) tll
            , benchBS  $ nf (BS.intercalate bsw) bsl
            , benchBSL $ nf (BL.intercalate blw) bll
            , benchS   $ nf (L.intercalate lw) sl
            ]
        , bgroup "intersperse"
            [ benchT   $ nf (T.intersperse c) ta
            , benchTL  $ nf (TL.intersperse c) tla
            , benchBS  $ nf (BS.intersperse c) bsa
            , benchBSL $ nf (BL.intersperse c) bla
            , benchS   $ nf (L.intersperse c) sa
            ]
        , bgroup "isInfixOf"
            [ benchT   $ nf (T.isInfixOf tsw) ta
            , benchTL  $ nf (TL.isInfixOf tlw) tla
            , benchBS  $ nf (BS.isInfixOf bsw) bsa
              -- no isInfixOf for lazy bytestrings
            , benchS   $ nf (L.isInfixOf lw) sa
            ]
        , bgroup "last"
            [ benchT   $ nf T.last ta
            , benchTL  $ nf TL.last tla
            , benchBS  $ nf BS.last bsa
            , benchBSL $ nf BL.last bla
            , benchS   $ nf L.last sa
            ]
        , bgroup "map"
            [ benchT   $ nf (T.map f) ta
            , benchTL  $ nf (TL.map f) tla
            , benchBS  $ nf (BS.map f) bsa
            , benchBSL $ nf (BL.map f) bla
            , benchS   $ nf (L.map f) sa
            ]
        , bgroup "mapAccumL"
            [ benchT   $ nf (T.mapAccumL g 0) ta
            , benchTL  $ nf (TL.mapAccumL g 0) tla
            , benchBS  $ nf (BS.mapAccumL g 0) bsa
            , benchBSL $ nf (BL.mapAccumL g 0) bla
            , benchS   $ nf (L.mapAccumL g 0) sa
            ]
        , bgroup "mapAccumR"
            [ benchT   $ nf (T.mapAccumR g 0) ta
            , benchTL  $ nf (TL.mapAccumR g 0) tla
            , benchBS  $ nf (BS.mapAccumR g 0) bsa
            , benchBSL $ nf (BL.mapAccumR g 0) bla
            , benchS   $ nf (L.mapAccumR g 0) sa
            ]
        , bgroup "map.map"
            [ benchT   $ nf (T.map f . T.map f) ta
            , benchTL  $ nf (TL.map f . TL.map f) tla
            , benchBS  $ nf (BS.map f . BS.map f) bsa
            , benchBSL $ nf (BL.map f . BL.map f) bla
            , benchS   $ nf (L.map f . L.map f) sa
            ]
        , bgroup "replicate char"
            [ benchT   $ nf (T.replicate bsa_len) (T.singleton c)
            , benchTL  $ nf (TL.replicate (fromIntegral bsa_len)) (TL.singleton c)
            , benchBS  $ nf (BS.replicate bsa_len) c
            , benchBSL $ nf (BL.replicate (fromIntegral bsa_len)) c
            , benchS   $ nf (L.replicate bsa_len) c
            ]
        , bgroup "replicate string"
            [ benchT   $ nf (T.replicate (bsa_len `div` T.length tsw)) tsw
            , benchTL  $ nf (TL.replicate (fromIntegral bsa_len `div` TL.length tlw)) tlw
            , benchS   $ nf (replicat (bsa_len `div` T.length tsw)) lw
            ]
        , bgroup "reverse"
            [ benchT   $ nf T.reverse ta
            , benchTL  $ nf TL.reverse tla
            , benchBS  $ nf BS.reverse bsa
            , benchBSL $ nf BL.reverse bla
            , benchS   $ nf L.reverse sa
            ]
        , bgroup "take"
            [ benchT   $ nf (T.take (ta_len `div` 3)) ta
            , benchTL  $ nf (TL.take (tla_len `div` 3)) tla
            , benchBS  $ nf (BS.take (bsa_len `div` 3)) bsa
            , benchBSL $ nf (BL.take (bla_len `div` 3)) bla
            , benchS   $ nf (L.take (sa_len `div` 3)) sa
            ]
        , bgroup "tail"
            [ benchT   $ nf T.tail ta
            , benchTL  $ nf TL.tail tla
            , benchBS  $ nf BS.tail bsa
            , benchBSL $ nf BL.tail bla
            , benchS   $ nf L.tail sa
            ]
        , bgroup "toLower"
            [ benchT   $ nf T.toLower ta
            , benchTL  $ nf TL.toLower tla
            , benchBS  $ nf (BS.map toLower) bsa
            , benchBSL $ nf (BL.map toLower) bla
            , benchS   $ nf (L.map toLower) sa
            ]
        , bgroup "toUpper"
            [ benchT   $ nf T.toUpper ta
            , benchTL  $ nf TL.toUpper tla
            , benchBS  $ nf (BS.map toUpper) bsa
            , benchBSL $ nf (BL.map toUpper) bla
            , benchS   $ nf (L.map toUpper) sa
            ]
        , bgroup "uncons"
            [ benchT   $ nf T.uncons ta
            , benchTL  $ nf TL.uncons tla
            , benchBS  $ nf BS.uncons bsa
            , benchBSL $ nf BL.uncons bla
            , benchS   $ nf L.uncons sa
            ]
        , bgroup "words"
            [ benchT   $ nf T.words ta
            , benchTL  $ nf TL.words tla
            , benchBS  $ nf BS.words bsa
            , benchBSL $ nf BL.words bla
            , benchS   $ nf L.words sa
            ]
        , bgroup "zipWith"
            [ benchT   $ nf (T.zipWith min tb) ta
            , benchTL  $ nf (TL.zipWith min tlb) tla
            , benchBS  $ nf (BS.zipWith min bsb) bsa
            , benchBSL $ nf (BL.zipWith min blb) bla
            , benchS   $ nf (L.zipWith min sb) sa
            ]
        , bgroup "length"
            [ bgroup "cons"
                [ benchT   $ nf (T.length . T.cons c) ta
                , benchTL  $ nf (TL.length . TL.cons c) tla
                , benchBS  $ nf (BS.length . BS.cons c) bsa
                , benchBSL $ nf (BL.length . BL.cons c) bla
                , benchS   $ nf (L.length . (:) c) sa
                ]
            , bgroup "decode"
                [ benchT   $ nf (T.length . T.decodeUtf8) bsa
                , benchTL  $ nf (TL.length . TL.decodeUtf8) bla
                , benchBS  $ nf (L.length . BS.unpack) bsa
                , benchBSL $ nf (L.length . BL.unpack) bla
                , bench "StringUTF8" $ nf (L.length . UTF8.toString) bsa
                ]
            , bgroup "drop"
                [ benchT   $ nf (T.length . T.drop (ta_len `div` 3)) ta
                , benchTL  $ nf (TL.length . TL.drop (tla_len `div` 3)) tla
                , benchBS  $ nf (BS.length . BS.drop (bsa_len `div` 3)) bsa
                , benchBSL $ nf (BL.length . BL.drop (bla_len `div` 3)) bla
                , benchS   $ nf (L.length . L.drop (sa_len `div` 3)) sa
                ]
            , bgroup "filter"
                [ benchT   $ nf (T.length . T.filter p0) ta
                , benchTL  $ nf (TL.length . TL.filter p0) tla
                , benchBS  $ nf (BS.length . BS.filter p0) bsa
                , benchBSL $ nf (BL.length . BL.filter p0) bla
                , benchS   $ nf (L.length . L.filter p0) sa
                ]
            , bgroup "filter.filter"
                [ benchT   $ nf (T.length . T.filter p1 . T.filter p0) ta
                , benchTL  $ nf (TL.length . TL.filter p1 . TL.filter p0) tla
                , benchBS  $ nf (BS.length . BS.filter p1 . BS.filter p0) bsa
                , benchBSL $ nf (BL.length . BL.filter p1 . BL.filter p0) bla
                , benchS   $ nf (L.length . L.filter p1 . L.filter p0) sa
                ]
            , bgroup "init"
                [ benchT   $ nf (T.length . T.init) ta
                , benchTL  $ nf (TL.length . TL.init) tla
                , benchBS  $ nf (BS.length . BS.init) bsa
                , benchBSL $ nf (BL.length . BL.init) bla
                , benchS   $ nf (L.length . L.init) sa
                ]
            , bgroup "intercalate"
                [ benchT   $ nf (T.length . T.intercalate tsw) tl
                , benchTL  $ nf (TL.length . TL.intercalate tlw) tll
                , benchBS  $ nf (BS.length . BS.intercalate bsw) bsl
                , benchBSL $ nf (BL.length . BL.intercalate blw) bll
                , benchS   $ nf (L.length . L.intercalate lw) sl
                ]
            , bgroup "intersperse"
                [ benchT   $ nf (T.length . T.intersperse c) ta
                , benchTL  $ nf (TL.length . TL.intersperse c) tla
                , benchBS  $ nf (BS.length . BS.intersperse c) bsa
                , benchBSL $ nf (BL.length . BL.intersperse c) bla
                , benchS   $ nf (L.length . L.intersperse c) sa
                ]
            , bgroup "map"
                [ benchT   $ nf (T.length . T.map f) ta
                , benchTL  $ nf (TL.length . TL.map f) tla
                , benchBS  $ nf (BS.length . BS.map f) bsa
                , benchBSL $ nf (BL.length . BL.map f) bla
                , benchS   $ nf (L.length . L.map f) sa
                ]
            , bgroup "map.map"
                [ benchT   $ nf (T.length . T.map f . T.map f) ta
                , benchTL  $ nf (TL.length . TL.map f . TL.map f) tla
                , benchBS  $ nf (BS.length . BS.map f . BS.map f) bsa
                , benchS   $ nf (L.length . L.map f . L.map f) sa
                ]
            , bgroup "replicate char"
                [ benchT   $ nf (T.length . T.replicate bsa_len) (T.singleton c)
                , benchTL  $ nf (TL.length . TL.replicate (fromIntegral bsa_len)) (TL.singleton c)
                , benchBS  $ nf (BS.length . BS.replicate bsa_len) c
                , benchBSL $ nf (BL.length . BL.replicate (fromIntegral bsa_len)) c
                , benchS   $ nf (L.length . L.replicate bsa_len) c
                ]
            , bgroup "replicate string"
                [ benchT   $ nf (T.length . T.replicate (bsa_len `div` T.length tsw)) tsw
                , benchTL  $ nf (TL.length . TL.replicate (fromIntegral bsa_len `div` TL.length tlw)) tlw
                , benchS   $ nf (L.length . replicat (bsa_len `div` T.length tsw)) lw
                ]
            , bgroup "take"
                [ benchT   $ nf (T.length . T.take (ta_len `div` 3)) ta
                , benchTL  $ nf (TL.length . TL.take (tla_len `div` 3)) tla
                , benchBS  $ nf (BS.length . BS.take (bsa_len `div` 3)) bsa
                , benchBSL $ nf (BL.length . BL.take (bla_len `div` 3)) bla
                , benchS   $ nf (L.length . L.take (sa_len `div` 3)) sa
                ]
            , bgroup "tail"
                [ benchT   $ nf (T.length . T.tail) ta
                , benchTL  $ nf (TL.length . TL.tail) tla
                , benchBS  $ nf (BS.length . BS.tail) bsa
                , benchBSL $ nf (BL.length . BL.tail) bla
                , benchS   $ nf (L.length . L.tail) sa
                ]
            , bgroup "toLower"
                [ benchT   $ nf (T.length . T.toLower) ta
                , benchTL  $ nf (TL.length . TL.toLower) tla
                , benchBS  $ nf (BS.length . BS.map toLower) bsa
                , benchBSL $ nf (BL.length . BL.map toLower) bla
                , benchS   $ nf (L.length . L.map toLower) sa
                ]
            , bgroup "toUpper"
                [ benchT   $ nf (T.length . T.toUpper) ta
                , benchTL  $ nf (TL.length . TL.toUpper) tla
                , benchBS  $ nf (BS.length . BS.map toUpper) bsa
                , benchBSL $ nf (BL.length . BL.map toUpper) bla
                , benchS   $ nf (L.length . L.map toUpper) sa
                ]
            , bgroup "words"
                [ benchT   $ nf (L.length . T.words) ta
                , benchTL  $ nf (L.length . TL.words) tla
                , benchBS  $ nf (L.length . BS.words) bsa
                , benchBSL $ nf (L.length . BL.words) bla
                , benchS   $ nf (L.length . L.words) sa
                ]
            , bgroup "zipWith"
                [ benchT   $ nf (T.length . T.zipWith min tb) ta
                , benchTL  $ nf (TL.length . TL.zipWith min tlb) tla
                , benchBS  $ nf (L.length . BS.zipWith min bsb) bsa
                , benchBSL $ nf (L.length . BL.zipWith min blb) bla
                , benchS   $ nf (L.length . L.zipWith min sb) sa
                ]
              ]
        , bgroup "Builder"
            [ bench "mappend char" $ nf (TL.length . TB.toLazyText . mappendNChar 'a') 10000
            , bench "mappend 8 char" $ nf (TL.length . TB.toLazyText . mappend8Char) 'a'
            , bench "mappend text" $ nf (TL.length . TB.toLazyText . mappendNText short) 10000
            ]
        ]
  where
    benchS   = bench ("String+" ++ kind)
    benchT   = bench ("Text+" ++ kind)
    benchTL  = bench ("LazyText+" ++ kind)
    benchBS  = bench ("ByteString+" ++ kind)
    benchBSL = bench ("LazyByteString+" ++ kind)

    c  = 'й'
    p0 = (== c)
    p1 = (/= 'д')
    lw  = "право"
    bsw  = UTF8.fromString lw
    blw  = BL.fromChunks [bsw]
    tsw  = T.pack lw
    tlw  = TL.fromChunks [tsw]
    f (C# c#) = C# (chr# (ord# c# +# 1#))
    g (I# i#) (C# c#) = (I# (i# +# 1#), C# (chr# (ord# c# +# i#)))
    len l _ = l + (1::Int)
    replicat n = concat . L.replicate n
    short = T.pack "short"

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData BS.ByteString

instance NFData BL.ByteString where
    rnf BL.Empty        = ()
    rnf (BL.Chunk _ ts) = rnf ts
#endif

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

-- | Split a bytestring in chunks
--
chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf k = go
  where
    go t = case BS.splitAt k t of
             (a,b) | BS.null a -> []
                   | otherwise -> a : go b

-- | Append a character n times
--
mappendNChar :: Char -> Int -> TB.Builder
mappendNChar c n = go 0
  where
    go i
      | i < n     = TB.singleton c `mappend` go (i+1)
      | otherwise = mempty

-- | Gives more opportunity for inlining and elimination of unnecesary
-- bounds checks.
--
mappend8Char :: Char -> TB.Builder
mappend8Char c = TB.singleton c `mappend` TB.singleton c `mappend`
                 TB.singleton c `mappend` TB.singleton c `mappend`
                 TB.singleton c `mappend` TB.singleton c `mappend`
                 TB.singleton c `mappend` TB.singleton c

-- | Append a text N times
--
mappendNText :: T.Text -> Int -> TB.Builder
mappendNText t n = go 0
  where
    go i
      | i < n     = TB.fromText t `mappend` go (i+1)
      | otherwise = mempty
