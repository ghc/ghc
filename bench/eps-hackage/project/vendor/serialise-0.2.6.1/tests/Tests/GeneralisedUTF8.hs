module Tests.GeneralisedUTF8 where

import GHC.Exts
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Codec.CBOR.ByteArray as BA
import Codec.Serialise.Internal.GeneralisedUTF8

testEncoder :: String -> Property
testEncoder s =
    case encodeGenUTF8 s of
      (ba, enc) ->
          toList ba === BS.unpack (T.encodeUtf8 $ T.pack s)
          .&&.
          enc === correctEnc
  where
    correctEnc
      | any isSurrogate s = GeneralisedUTF8
      | otherwise         = ConformantUTF8

testDecoder :: String -> Property
testDecoder s =
    decodeGenUTF8 ba === s
  where
    BA.BA ba = BA.fromByteString $ T.encodeUtf8 $ T.pack s

testTree :: TestTree
testTree =
    testGroup "Generalised UTF-8 codec"
        [ testProperty "encoder" testEncoder
        , testProperty "decoder" testDecoder
        ]
