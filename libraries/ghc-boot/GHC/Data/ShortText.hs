{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
-- An Unicode string for internal GHC use. Meant to replace String
-- in places where being a lazy linked is not very useful and a more
-- memory efficient data structure is desirable.

-- Very similar to FastString, but not hash-consed and with some extra instances and
-- functions for serialisation and I/O. Should be imported qualified.

module GHC.Data.ShortText where

import Prelude

import Data.Binary
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as SBS
import GHC.Exts
import GHC.IO.Unsafe
import GHC.Utils.Encoding
import System.FilePath (isPathSeparator)

newtype ShortText = ShortText { contents :: SBS.ShortByteString
                              }
                              deriving stock (Show)
                              deriving newtype (Eq, Ord, Binary, Semigroup, Monoid)

-- We don't want to derive this one from ShortByteString since that one won't handle
-- UTF-8 characters correctly
instance IsString ShortText where
  fromString = pack

charLength :: ShortText -> Int
charLength st = unsafeDupablePerformIO $ countUTF8Chars (contents st)
byteLength :: ShortText -> Int
byteLength st = SBS.length $ contents st

pack :: String -> ShortText
pack s = unsafeDupablePerformIO $ ShortText <$> utf8EncodeShortByteString s

unpack :: ShortText -> String
unpack st = utf8DecodeShortByteString $ contents st

null :: ShortText -> Bool
null st = SBS.null $ contents st

-- This seems dangerous, but since the path separators are in the ASCII set they map down
-- to a single byte when encoded in UTF-8 and so this should work even when casting to ByteString.
splitFilePath :: ShortText -> [ShortText]
splitFilePath st = map (ShortText . SBS.toShort) $ B8.splitWith isPathSeparator st'
  where st' = SBS.fromShort $ contents st

head :: ShortText -> Char
head st
  | SBS.null $ contents st = error "head: Empty ShortText"
  | otherwise              = Prelude.head $ unpack st

stripPrefix :: ShortText -> ShortText -> Maybe ShortText
stripPrefix prefix st = case B8.stripPrefix (SBS.fromShort $ contents prefix) (SBS.fromShort $ contents st) of
  Nothing -> Nothing
  Just rest -> Just $ ShortText $ SBS.toShort rest
