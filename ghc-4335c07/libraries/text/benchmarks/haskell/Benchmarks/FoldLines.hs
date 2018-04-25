-- | Read a file line-by-line using handles, and perform a fold over the lines.
-- The fold is used here to calculate the number of lines in the file.
--
-- Tested in this benchmark:
--
-- * Buffered, line-based IO
--
{-# LANGUAGE BangPatterns #-}
module Benchmarks.FoldLines
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, whnfIO)
import System.IO
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

benchmark :: FilePath -> IO Benchmark
benchmark fp = return $ bgroup "ReadLines"
    [ bench "Text"       $ withHandle $ foldLinesT (\n _ -> n + 1) (0 :: Int)
    , bench "ByteString" $ withHandle $ foldLinesB (\n _ -> n + 1) (0 :: Int)
    ]
  where
    withHandle f = whnfIO $ do
        h <- openFile fp ReadMode
        hSetBuffering h (BlockBuffering (Just 16384))
        x <- f h
        hClose h
        return x

-- | Text line fold
--
foldLinesT :: (a -> T.Text -> a) -> a -> Handle -> IO a
foldLinesT f z0 h = go z0
  where
    go !z = do
        eof <- hIsEOF h
        if eof
            then return z
            else do
                l <- T.hGetLine h
                let z' = f z l in go z'
{-# INLINE foldLinesT #-}

-- | ByteString line fold
--
foldLinesB :: (a -> B.ByteString -> a) -> a -> Handle -> IO a
foldLinesB f z0 h = go z0
  where
    go !z = do
        eof <- hIsEOF h
        if eof
            then return z
            else do
                l <- B.hGetLine h
                let z' = f z l in go z'
{-# INLINE foldLinesB #-}
