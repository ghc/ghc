-- | UTF-8 encode a text
--
-- Tested in this benchmark:
--
-- * Replicating a string a number of times
--
-- * UTF-8 encoding it
--
module Benchmarks.EncodeUtf8
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, whnf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

benchmark :: String -> IO Benchmark
benchmark string = do
    return $ bgroup "EncodeUtf8"
        [ bench "Text"     $ whnf (B.length . T.encodeUtf8)   text
        , bench "LazyText" $ whnf (BL.length . TL.encodeUtf8) lazyText
        ]
  where
    -- The string in different formats
    text = T.replicate k $ T.pack string
    lazyText = TL.replicate (fromIntegral k) $ TL.pack string

    -- Amount
    k = 100000
