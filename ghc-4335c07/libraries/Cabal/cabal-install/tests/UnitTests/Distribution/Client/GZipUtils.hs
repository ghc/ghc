module UnitTests.Distribution.Client.GZipUtils (
  tests
  ) where

import Codec.Compression.GZip          as GZip
import Codec.Compression.Zlib          as Zlib
import Control.Exception.Base                  (evaluate)
import Control.Exception                       (try, SomeException)
import Control.Monad                           (void)
import Data.ByteString                as BS    (null)
import Data.ByteString.Lazy           as BSL   (pack, toChunks)
import Data.ByteString.Lazy.Char8     as BSLL  (pack, init, length)
import Data.Monoid                             ((<>))
import Distribution.Client.GZipUtils           (maybeDecompress)
import Data.Word                               (Word8)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests = [ testCase "maybeDecompress" maybeDecompressUnitTest
        -- "decompress plain" property is non-trivial to state,
        -- maybeDecompress returns input bytestring only if error occurs right at the beginning of the decompression process
        -- generating such input would essentially duplicate maybeDecompress implementation
        , testProperty "decompress zlib"  prop_maybeDecompress_zlib
        , testProperty "decompress gzip"  prop_maybeDecompress_gzip
        ]

maybeDecompressUnitTest :: Assertion
maybeDecompressUnitTest =
        assertBool "decompress plain"            (maybeDecompress original              == original)
     >> assertBool "decompress zlib (with show)" (show (maybeDecompress compressedZlib) == show original)
     >> assertBool "decompress gzip (with show)" (show (maybeDecompress compressedGZip) == show original)
     >> assertBool "decompress zlib"             (maybeDecompress compressedZlib        == original)
     >> assertBool "decompress gzip"             (maybeDecompress compressedGZip        == original)
     >> assertBool "have no empty chunks"        (Prelude.all (not . BS.null) . BSL.toChunks . maybeDecompress $ compressedZlib)
     >> (runBrokenStream >>= assertBool "decompress broken stream" . isLeft)
  where
    original = BSLL.pack "original uncompressed input"
    compressedZlib = Zlib.compress original
    compressedGZip = GZip.compress original

    runBrokenStream :: IO (Either SomeException ())
    runBrokenStream = try . void . evaluate . BSLL.length $ maybeDecompress (BSLL.init compressedZlib <> BSLL.pack "*")

prop_maybeDecompress_zlib :: [Word8] -> Property
prop_maybeDecompress_zlib ws = property $ maybeDecompress compressedZlib === original
  where original = BSL.pack ws
        compressedZlib = Zlib.compress original

prop_maybeDecompress_gzip :: [Word8] -> Property
prop_maybeDecompress_gzip ws = property $ maybeDecompress compressedGZip === original
  where original = BSL.pack ws
        compressedGZip = GZip.compress original

-- (Only available from "Data.Either" since 7.8.)
isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left _) = True
