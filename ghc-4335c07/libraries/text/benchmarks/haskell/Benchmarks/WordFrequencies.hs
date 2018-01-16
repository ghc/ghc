-- | A word frequency count using the different string types
--
-- Tested in this benchmark:
--
-- * Splitting into words
--
-- * Converting to lowercase
--
-- * Comparing: Eq/Ord instances
--
module Benchmarks.WordFrequencies
    ( benchmark
    ) where

import Criterion (Benchmark, bench, bgroup, whnf)
import Data.Char (toLower)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

benchmark :: FilePath -> IO Benchmark
benchmark fp = do
    s <- readFile fp
    b <- B.readFile fp
    t <- T.readFile fp
    return $ bgroup "WordFrequencies"
        [ bench "String"     $ whnf (frequencies . words . map toLower)     s
        , bench "ByteString" $ whnf (frequencies . B.words . B.map toLower) b
        , bench "Text"       $ whnf (frequencies . T.words . T.toLower)     t
        ]

frequencies :: Ord a => [a] -> Map a Int
frequencies = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty
