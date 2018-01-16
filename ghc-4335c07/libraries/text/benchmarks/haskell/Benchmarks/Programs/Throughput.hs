-- | This benchmark simply reads and writes a file using the various string
-- libraries. The point of it is that we can make better estimations on how
-- much time the other benchmarks spend doing IO.
--
-- Note that we expect ByteStrings to be a whole lot faster, since they do not
-- do any actual encoding/decoding here, while String and Text do have UTF-8
-- encoding/decoding.
--
-- Tested in this benchmark:
--
-- * Reading the file
--
-- * Replacing text between HTML tags (<>) with whitespace
--
-- * Writing back to a handle
--
module Benchmarks.Programs.Throughput
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, whnfIO)
import System.IO (Handle, hPutStr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL

benchmark :: FilePath -> Handle -> IO Benchmark
benchmark fp sink = return $ bgroup "Throughput"
    [ bench "String" $ whnfIO $ readFile fp >>= hPutStr sink
    , bench "ByteString" $ whnfIO $ B.readFile fp >>= B.hPutStr sink
    , bench "LazyByteString" $ whnfIO $ BL.readFile fp >>= BL.hPutStr sink
    , bench "Text" $ whnfIO $ T.readFile fp >>= T.hPutStr sink
    , bench "LazyText" $ whnfIO $ TL.readFile fp >>= TL.hPutStr sink
    , bench "TextByteString" $ whnfIO $
        B.readFile fp >>= B.hPutStr sink . T.encodeUtf8 .  T.decodeUtf8
    , bench "LazyTextByteString" $ whnfIO $
        BL.readFile fp >>= BL.hPutStr sink . TL.encodeUtf8 . TL.decodeUtf8
    ]
