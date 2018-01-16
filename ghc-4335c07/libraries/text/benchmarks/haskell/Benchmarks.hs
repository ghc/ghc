-- | Main module to run the micro benchmarks
--
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Criterion.Main (Benchmark, defaultMain, bgroup)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), openFile, hSetEncoding, utf8)

import qualified Benchmarks.Builder as Builder
import qualified Benchmarks.Concat as Concat
import qualified Benchmarks.DecodeUtf8 as DecodeUtf8
import qualified Benchmarks.EncodeUtf8 as EncodeUtf8
import qualified Benchmarks.Equality as Equality
import qualified Benchmarks.FileRead as FileRead
import qualified Benchmarks.FoldLines as FoldLines
import qualified Benchmarks.Mul as Mul
import qualified Benchmarks.Pure as Pure
import qualified Benchmarks.ReadNumbers as ReadNumbers
import qualified Benchmarks.Replace as Replace
import qualified Benchmarks.Search as Search
import qualified Benchmarks.Stream as Stream
import qualified Benchmarks.WordFrequencies as WordFrequencies

import qualified Benchmarks.Programs.BigTable as Programs.BigTable
import qualified Benchmarks.Programs.Cut as Programs.Cut
import qualified Benchmarks.Programs.Fold as Programs.Fold
import qualified Benchmarks.Programs.Sort as Programs.Sort
import qualified Benchmarks.Programs.StripTags as Programs.StripTags
import qualified Benchmarks.Programs.Throughput as Programs.Throughput

main :: IO ()
main = benchmarks >>= defaultMain

benchmarks :: IO [Benchmark]
benchmarks = do
    sink <- openFile "/dev/null" WriteMode
    hSetEncoding sink utf8

    -- Traditional benchmarks
    bs <- sequence
        [ Builder.benchmark
        , Concat.benchmark
        , DecodeUtf8.benchmark "html" (tf "libya-chinese.html")
        , DecodeUtf8.benchmark "xml" (tf "yiwiki.xml")
        , DecodeUtf8.benchmark "ascii" (tf "ascii.txt")
        , DecodeUtf8.benchmark "russian" (tf "russian.txt")
        , DecodeUtf8.benchmark "japanese" (tf "japanese.txt")
        , EncodeUtf8.benchmark "επανάληψη 竺法蘭共譯"
        , Equality.benchmark (tf "japanese.txt")
        , FileRead.benchmark (tf "russian.txt")
        , FoldLines.benchmark (tf "russian.txt")
        , Mul.benchmark
        , Pure.benchmark "tiny" (tf "tiny.txt")
        , Pure.benchmark "ascii" (tf "ascii-small.txt")
        -- , Pure.benchmark "france" (tf "france.html")
        , Pure.benchmark "russian" (tf "russian-small.txt")
        , Pure.benchmark "japanese" (tf "japanese.txt")
        , ReadNumbers.benchmark (tf "numbers.txt")
        , Replace.benchmark (tf "russian.txt") "принимая" "своем"
        , Search.benchmark (tf "russian.txt") "принимая"
        , Stream.benchmark (tf "russian.txt")
        , WordFrequencies.benchmark (tf "russian.txt")
        ]

    -- Program-like benchmarks
    ps <- bgroup "Programs" `fmap` sequence
        [ Programs.BigTable.benchmark sink
        , Programs.Cut.benchmark (tf "russian.txt") sink 20 40
        , Programs.Fold.benchmark (tf "russian.txt") sink
        , Programs.Sort.benchmark (tf "russian.txt") sink
        , Programs.StripTags.benchmark (tf "yiwiki.xml") sink
        , Programs.Throughput.benchmark (tf "russian.txt") sink
        ]

    return $ bs ++ [ps]
  where
    -- Location of a test file
    tf = ("../tests/text-test-data" </>)
