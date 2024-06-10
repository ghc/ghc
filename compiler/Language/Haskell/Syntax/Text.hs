-- | The string type used for text in the Haskell AST (string literals, FFI
-- names, ...).
--
-- We use 'HText' rather than 'Data.Text.Text' directly to ensure 'String's
-- parsed from the source program are not lossily converted.
--
-- Specifically, 'Data.Text' functions replace out surrogate characters (which
-- are valid Haskell 'Char's and can occur in source text), thus we must avoid
-- them when dealing with source text:
--
--   > A 'Data.Text.Text' value is a sequence of Unicode scalar values [...]. As
--   > such, a 'Data.Text.Text' cannot contain values in the range U+D800 to
--   > U+DFFF inclusive. Haskell implementations admit all Unicode code points
--   > (§3.4, definition D10) as 'Char' values, including code points from this
--   > invalid range. This means that there are some 'Char' values (corresponding
--   > to Surrogate category) that are not valid Unicode scalar values [...].
--
-- 'HText' is 'GHC.Data.ShortText.ShortText', a datatype which uses GHC's
-- Modified UTF-8 encoding. That encoding preserves surrogates and NUL
-- which is necessary to represent Haskell's source 'String's
module Language.Haskell.Syntax.Text
  ( HText
  , packHText
  , unpackHText
  , bytesHText
  , shortByteStringToHText
    -- * Utils
  , lengthHText, nullHText
  ) where

import Prelude

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS

import GHC.Data.ShortText (ShortText(..))
import qualified GHC.Data.ShortText as ST

-- | Text in the Haskell AST. See the module header for why this is not just
-- 'Data.Text.Text': the bytes are GHC's Modified UTF-8 so that arbitrary code
-- points (including surrogates and NUL) round-trip.
type HText = ShortText

-- | Encode a 'String' into an 'HText'.
packHText :: String -> HText
packHText = ST.pack

-- | Decode an 'HText' back into its 'String' of code points.
-- Inverse of 'packHText'.
unpackHText :: HText -> String
unpackHText = ST.unpack

-- | Construct a 'BS.ByteString' from the underlying Modified UTF-8 buffer as-is.
bytesHText :: HText -> BS.ByteString
bytesHText = SBS.fromShort . contents

-- | Wrap a 'ShortByteString' of Modified UTF-8 bytes as an 'HText'.
-- **Unsafe**: the 'ShortByteString' must be valid Modified UTF-8.
shortByteStringToHText :: ShortByteString -> HText
shortByteStringToHText = ShortText

--------------------------------------------------------------------------------

-- | The number of code points in an 'HText'.
lengthHText :: HText -> Int
lengthHText = ST.codepointLength

-- | Test whether an 'HText' is empty.
nullHText :: HText -> Bool
nullHText = ST.null
