{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Represents a small chunk of UTF8 text from a source code file.
-}
module Language.Haskell.Textual.UTF8
   (
   -- * Data-type
     TextUTF8()
   -- ** Construction
   , encodeUTF8
   , unsafeFromShortByteString
   -- ** Deconstruction
   , bytesUTF8
   , decodeUTF8
   , headUTF8
   -- ** Transformation
   , linesUTF8
   , unlinesUTF8
   ) where


import Prelude

import Control.DeepSeq
import Data.ByteString.Short (ShortByteString(..))
import qualified Data.ByteString.Short as SBS
import Data.Data
import Data.Foldable (toList)
import Data.String (IsString(..))
import Data.Word (Word8)

-- These is a modules are components of the @base@ package,
-- hence they do not directly couple the library to GHC.
import GHC.Base (Char(C#))
import GHC.Encoding.UTF8

{- |
A UTF8 encoded ShortByteString representing the textual snippet of code
associated with a given element of the abstract syntax tree.
-}
newtype TextUTF8 = TextUTF8 { bytesUTF8 :: ShortByteString }
    deriving (Data, Eq, Ord)

instance IsString TextUTF8 where
  fromString = encodeUTF8

instance NFData TextUTF8 where
  rnf (TextUTF8 !sbs) = rnf sbs

instance Semigroup TextUTF8 where
  (TextUTF8 x) <> (TextUTF8 y) = TextUTF8 $ x <> y

instance Monoid TextUTF8 where
  mempty = TextUTF8 mempty

instance Show TextUTF8 where
  show (TextUTF8 sbs) = utf8DecodeShortByteString sbs

{- |
Decode a UTF8 chunk of text to a 'String'.
-}
{-# INLINE decodeUTF8 #-}
decodeUTF8 :: TextUTF8 -> String
decodeUTF8 = utf8DecodeShortByteString . bytesUTF8

{- |
Encode a 'String' as a UTF8 chunk of text.
-}
{-# INLINE encodeUTF8 #-}
encodeUTF8 :: String -> TextUTF8
encodeUTF8 = TextUTF8 . utf8EncodeShortByteString

{- |
Extract the first code point from the UTF8 check of text.

_Time:_ $\mathcal{O}\left( 1 \right )$
-}
headUTF8 :: TextUTF8 -> Maybe Char
headUTF8 (TextUTF8 sbs@(SBS ba#))
  | SBS.length sbs == 0 = Nothing
  | otherwise =
    let (# c#, _ #) = utf8DecodeCharByteArray# ba# 0#
    in  Just $ C# c#

{- |
Split a UTF8 fragment of text on newline characters (@'\n'@).
-}
linesUTF8 :: TextUTF8 -> [TextUTF8]
linesUTF8 = fmap TextUTF8 . SBS.split newlineByte . bytesUTF8

{- |
Join a collection of UTF8 text fragments with newline characters (@'\n'@).
-}
unlinesUTF8 :: Foldable f => f TextUTF8 -> TextUTF8
unlinesUTF8 =
  TextUTF8 . SBS.intercalate (SBS.singleton newlineByte) . fmap bytesUTF8 . toList 

{- |
Assumes that the shortByteString is already UTF8 encoded.

/This precondition is not checked!/
-}
{-# INLINE unsafeFromShortByteString #-}
unsafeFromShortByteString :: ShortByteString -> TextUTF8
unsafeFromShortByteString = TextUTF8

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Internal Functionality
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newlineByte :: Word8
newlineByte = 0x0A -- 0x0A (10) is the new line character (\n)

utf8DecodeShortByteString :: ShortByteString -> [Char]
utf8DecodeShortByteString (SBS ba#) = utf8DecodeByteArray# ba#

utf8EncodeShortByteString :: String -> ShortByteString
utf8EncodeShortByteString str = SBS (utf8EncodeByteArray# str)
