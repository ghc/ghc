{-
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
-}

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
   , decodeUTF8
   , bytesUTF8
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

-- TODO: This will need to be internalized
import GHC.Utils.Encoding.UTF8

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

newlineByte :: Word8
newlineByte = 0x0A -- 0x0A (10) is the new line character (\n)
