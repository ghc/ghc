-- | This module contains a number of benchmarks for the different streaming
-- functions
--
-- Tested in this benchmark:
--
-- * Most streaming functions
--
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Benchmarks.Stream
    ( benchmark
    ) where

import Control.DeepSeq (NFData (..))
import Criterion (Benchmark, bgroup, bench, nf)
import Data.Text.Internal.Fusion.Types (Step (..), Stream (..))
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as E
import qualified Data.Text.Internal.Encoding.Fusion as T
import qualified Data.Text.Internal.Encoding.Fusion.Common as F
import qualified Data.Text.Internal.Fusion as F
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Internal.Lazy.Encoding.Fusion as TL
import qualified Data.Text.Internal.Lazy.Fusion as FL
import qualified Data.Text.Lazy.IO as TL

instance NFData a => NFData (Stream a) where
    -- Currently, this implementation does not force evaluation of the size hint
    rnf (Stream next s0 _) = go s0
      where
        go !s = case next s of
            Done       -> ()
            Skip s'    -> go s'
            Yield x s' -> rnf x `seq` go s'

benchmark :: FilePath -> IO Benchmark
benchmark fp = do
    -- Different formats
    t  <- T.readFile fp
    let !utf8    = T.encodeUtf8 t
        !utf16le = T.encodeUtf16LE t
        !utf16be = T.encodeUtf16BE t
        !utf32le = T.encodeUtf32LE t
        !utf32be = T.encodeUtf32BE t

    -- Once again for the lazy variants
    tl <- TL.readFile fp
    let !utf8L    = TL.encodeUtf8 tl
        !utf16leL = TL.encodeUtf16LE tl
        !utf16beL = TL.encodeUtf16BE tl
        !utf32leL = TL.encodeUtf32LE tl
        !utf32beL = TL.encodeUtf32BE tl

    -- For the functions which operate on streams
    let !s = F.stream t

    return $ bgroup "Stream"

        -- Fusion
        [ bgroup "stream" $
            [ bench "Text"     $ nf F.stream t
            , bench "LazyText" $ nf FL.stream tl
            ]
          -- must perform exactly the same as stream above due to
          -- stream/unstream (i.e. stream after unstream) fusion
        , bgroup "stream-fusion" $
            [ bench "Text"     $ nf (F.stream . F.unstream . F.stream) t
            , bench "LazyText" $ nf (FL.stream . FL.unstream . FL.stream) tl
            ]
          -- measure the overhead of unstream after stream
        , bgroup "stream-unstream" $
            [ bench "Text"     $ nf (F.unstream . F.stream) t
            , bench "LazyText" $ nf (FL.unstream . FL.stream) tl
            ]

        -- Encoding.Fusion
        , bgroup "streamUtf8"
            [ bench "Text"     $ nf (T.streamUtf8 E.lenientDecode) utf8
            , bench "LazyText" $ nf (TL.streamUtf8 E.lenientDecode) utf8L
            ]
        , bgroup "streamUtf16LE"
            [ bench "Text"     $ nf (T.streamUtf16LE E.lenientDecode) utf16le
            , bench "LazyText" $ nf (TL.streamUtf16LE E.lenientDecode) utf16leL
            ]
        , bgroup "streamUtf16BE"
            [ bench "Text"     $ nf (T.streamUtf16BE E.lenientDecode) utf16be
            , bench "LazyText" $ nf (TL.streamUtf16BE E.lenientDecode) utf16beL
            ]
        , bgroup "streamUtf32LE"
            [ bench "Text"     $ nf (T.streamUtf32LE E.lenientDecode) utf32le
            , bench "LazyText" $ nf (TL.streamUtf32LE E.lenientDecode) utf32leL
            ]
        , bgroup "streamUtf32BE"
            [ bench "Text"     $ nf (T.streamUtf32BE E.lenientDecode) utf32be
            , bench "LazyText" $ nf (TL.streamUtf32BE E.lenientDecode) utf32beL
            ]

        -- Encoding.Fusion.Common
        , bench "restreamUtf16LE" $ nf F.restreamUtf16LE s
        , bench "restreamUtf16BE" $ nf F.restreamUtf16BE s
        , bench "restreamUtf32LE" $ nf F.restreamUtf32LE s
        , bench "restreamUtf32BE" $ nf F.restreamUtf32BE s
        ]
