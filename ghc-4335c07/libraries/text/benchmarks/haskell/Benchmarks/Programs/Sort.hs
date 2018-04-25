-- | This benchmark sorts the lines of a file, like the @sort@ unix utility.
--
-- Tested in this benchmark:
--
-- * Reading the file
--
-- * Splitting into lines
--
-- * Sorting the lines
--
-- * Joining the lines
--
-- * Writing back to a handle
--
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Programs.Sort
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, whnfIO)
import Data.Monoid (mconcat)
import System.IO (Handle, hPutStr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL

benchmark :: FilePath -> Handle -> IO Benchmark
benchmark i o = return $ bgroup "Sort"
    [ bench "String" $ whnfIO $ readFile i >>= hPutStr o . string
    , bench "ByteString" $ whnfIO $ B.readFile i >>= B.hPutStr o . byteString
    , bench "LazyByteString" $ whnfIO $
      BL.readFile i >>= BL.hPutStr o . lazyByteString
    , bench "Text" $ whnfIO $ T.readFile i >>= T.hPutStr o . text
    , bench "LazyText" $ whnfIO $ TL.readFile i >>= TL.hPutStr o . lazyText
    , bench "TextByteString" $ whnfIO $ B.readFile i >>=
        B.hPutStr o . T.encodeUtf8 . text . T.decodeUtf8
    , bench "LazyTextByteString" $ whnfIO $ BL.readFile i >>=
        BL.hPutStr o . TL.encodeUtf8 . lazyText .  TL.decodeUtf8
    , bench "TextBuilder" $ whnfIO $ B.readFile i >>=
        BL.hPutStr o . TL.encodeUtf8 . textBuilder . T.decodeUtf8
    ]

string :: String -> String
string = unlines . L.sort . lines

byteString :: B.ByteString -> B.ByteString
byteString = BC.unlines . L.sort . BC.lines

lazyByteString :: BL.ByteString -> BL.ByteString
lazyByteString = BLC.unlines . L.sort . BLC.lines

text :: T.Text -> T.Text
text = T.unlines . L.sort . T.lines

lazyText :: TL.Text -> TL.Text
lazyText = TL.unlines . L.sort . TL.lines

-- | Text variant using a builder monoid for the final concatenation
--
textBuilder :: T.Text -> TL.Text
textBuilder = TLB.toLazyText . mconcat . L.intersperse (TLB.singleton '\n') .
    map TLB.fromText . L.sort . T.lines
