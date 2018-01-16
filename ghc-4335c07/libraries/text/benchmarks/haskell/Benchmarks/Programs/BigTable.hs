-- | Create a large HTML table and dump it to a handle
--
-- Tested in this benchmark:
--
-- * Creating a large HTML document using a builder
--
-- * Writing to a handle
--
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Programs.BigTable
    ( benchmark
    ) where

import Criterion (Benchmark, bench, whnfIO)
import Data.Monoid (mappend, mconcat)
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Data.Text.Lazy.IO (hPutStr)
import System.IO (Handle)
import qualified Data.Text as T

benchmark :: Handle -> IO Benchmark
benchmark sink = return $ bench "BigTable" $ whnfIO $ do
    hPutStr sink "Content-Type: text/html\n\n<table>"
    hPutStr sink . toLazyText . makeTable =<< rows
    hPutStr sink "</table>"
  where
    -- We provide the number of rows in IO so the builder value isn't shared
    -- between the benchmark samples.
    rows :: IO Int
    rows = return 20000
    {-# NOINLINE rows #-}

makeTable :: Int -> Builder
makeTable n = mconcat $ replicate n $ mconcat $ map makeCol [1 .. 50]

makeCol :: Int -> Builder
makeCol 1 = fromText "<tr><td>1</td>"
makeCol 50 = fromText "<td>50</td></tr>"
makeCol i = fromText "<td>" `mappend` (fromInt i `mappend` fromText "</td>")

fromInt :: Int -> Builder
fromInt = fromText . T.pack . show
