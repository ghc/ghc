-- | Search for a pattern in a file, find the number of occurences
--
-- Tested in this benchmark:
--
-- * Searching all occurences of a pattern using library routines
--
module Benchmarks.Search
    ( benchmark
    ) where

import Criterion (Benchmark, bench, bgroup, whnf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Search as BL
import qualified Data.ByteString.Search as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

benchmark :: FilePath -> T.Text -> IO Benchmark
benchmark fp needleT = do
    b  <- B.readFile fp
    bl <- BL.readFile fp
    t  <- T.readFile fp
    tl <- TL.readFile fp
    return $ bgroup "FileIndices"
        [ bench "ByteString"     $ whnf (byteString needleB)     b
        , bench "LazyByteString" $ whnf (lazyByteString needleB) bl
        , bench "Text"           $ whnf (text needleT)           t
        , bench "LazyText"       $ whnf (lazyText needleTL)      tl
        ]
  where
    needleB = T.encodeUtf8 needleT
    needleTL = TL.fromChunks [needleT]

byteString :: B.ByteString -> B.ByteString -> Int
byteString needle = length . B.indices needle

lazyByteString :: B.ByteString -> BL.ByteString -> Int
lazyByteString needle = length . BL.indices needle

text :: T.Text -> T.Text -> Int
text = T.count

lazyText :: TL.Text -> TL.Text -> Int
lazyText needle = fromIntegral . TL.count needle
