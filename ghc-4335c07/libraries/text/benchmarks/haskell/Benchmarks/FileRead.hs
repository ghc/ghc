-- | Benchmarks simple file reading
--
-- Tested in this benchmark:
--
-- * Reading a file from the disk
--
module Benchmarks.FileRead
    ( benchmark
    ) where

import Control.Applicative ((<$>))
import Criterion (Benchmark, bgroup, bench, whnfIO)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT

benchmark :: FilePath -> IO Benchmark
benchmark p = return $ bgroup "FileRead"
    [ bench "String" $ whnfIO $ length <$> readFile p
    , bench "ByteString" $ whnfIO $ SB.length <$> SB.readFile p
    , bench "LazyByteString" $ whnfIO $ LB.length <$> LB.readFile p
    , bench "Text" $ whnfIO $ T.length <$> T.readFile p
    , bench "LazyText" $ whnfIO $ LT.length <$> LT.readFile p
    , bench "TextByteString" $ whnfIO $
        (T.length . T.decodeUtf8) <$> SB.readFile p
    , bench "LazyTextByteString" $ whnfIO $
        (LT.length . LT.decodeUtf8) <$> LB.readFile p
    ]
