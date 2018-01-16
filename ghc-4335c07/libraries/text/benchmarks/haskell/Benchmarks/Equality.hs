-- | Compare a string with a copy of itself that is identical except
-- for the last character.
--
-- Tested in this benchmark:
--
-- * Comparison of strings (Eq instance)
--
module Benchmarks.Equality
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, whnf)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

benchmark :: FilePath -> IO Benchmark
benchmark fp = do
  b <- B.readFile fp
  bl1 <- BL.readFile fp
  -- A lazy bytestring is a list of chunks. When we do not explicitly create two
  -- different lazy bytestrings at a different address, the bytestring library
  -- will compare the chunk addresses instead of the chunk contents. This is why
  -- we read the lazy bytestring twice here.
  bl2 <- BL.readFile fp
  l <- readFile fp
  let t  = T.decodeUtf8 b
      tl = TL.decodeUtf8 bl1
  return $ bgroup "Equality"
    [ bench "Text" $ whnf (== T.init t `T.snoc` '\xfffd') t
    , bench "LazyText" $ whnf (== TL.init tl `TL.snoc` '\xfffd') tl
    , bench "ByteString" $ whnf (== B.init b `B.snoc` '\xfffd') b
    , bench "LazyByteString" $ whnf (== BL.init bl2 `BL.snoc` '\xfffd') bl1
    , bench "String" $ whnf (== init l ++ "\xfffd") l
    ]
