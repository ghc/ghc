-- | Program to replace HTML tags by whitespace
--
-- This program was originally contributed by Petr Prokhorenkov.
--
-- Tested in this benchmark:
--
-- * Reading the file
--
-- * Replacing text between HTML tags (<>) with whitespace
--
-- * Writing back to a handle
--
{-# OPTIONS_GHC -fspec-constr-count=5 #-}
module Benchmarks.Programs.StripTags
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, whnfIO)
import Data.List (mapAccumL)
import System.IO (Handle, hPutStr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

benchmark :: FilePath -> Handle -> IO Benchmark
benchmark i o = return $ bgroup "StripTags"
    [ bench "String" $ whnfIO $ readFile i >>= hPutStr o . string
    , bench "ByteString" $ whnfIO $ B.readFile i >>= B.hPutStr o . byteString
    , bench "Text" $ whnfIO $ T.readFile i >>= T.hPutStr o . text
    , bench "TextByteString" $ whnfIO $
        B.readFile i >>= B.hPutStr o . T.encodeUtf8 . text . T.decodeUtf8
    ]

string :: String -> String
string = snd . mapAccumL step 0

text :: T.Text -> T.Text
text = snd . T.mapAccumL step 0

byteString :: B.ByteString -> B.ByteString
byteString = snd . BC.mapAccumL step 0

step :: Int -> Char -> (Int, Char)
step d c
    | d > 0 || d' > 0 = (d', ' ')
    | otherwise       = (d', c)
  where
    d' = d + depth c
    depth '>' = 1
    depth '<' = -1
    depth _   = 0
