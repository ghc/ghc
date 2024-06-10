{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}

-- | An Unicode string for internal GHC use. Meant to replace String
-- in places where being a lazy linked is not very useful and a more
-- memory efficient data structure is desirable.

-- Very similar to FastString, but not hash-consed and with some extra instances and
-- functions for serialisation and I/O. Should be imported qualified.
--
-- /Note:/ This string is stored in Modified UTF8 format,
-- thus it's not byte-compatible with @ShortText@ type in @text-short@
-- package.

module GHC.Data.ShortText (
        -- * ShortText
        ShortText(..),
        -- ** Conversion to and from String
        singleton,
        pack,
        unpack,
        -- ** Operations
        codepointLength,
        byteLength,
        GHC.Data.ShortText.null,
        splitFilePath,
        GHC.Data.ShortText.head,
        stripPrefix
  ) where

import Prelude

import Control.Monad (guard)
import Control.DeepSeq as DeepSeq
import Data.Data
import Data.Binary
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short.Internal as SBS
import GHC.Exts
import GHC.IO
import GHC.Utils.Encoding
import System.FilePath (isPathSeparator)
import Data.Function

{-| A 'ShortText' is a modified UTF-8 encoded string meant for short strings like
file paths, module descriptions, etc.

GHC's Modified UTF-8 encoding diverges from Standard UTF-8 because:

1. It uses the overlong encoding for NUL characters 0xC080, to avoid issues with C
  code that assumes NUL terminated strings.

2. It allows arbitrary (and unpaired) surrogate values (generalised UTF-8, see
  e.g. [this page](https://wtf-8.codeberg.page/#generalized-utf8)).

Or in summary it is a "generalised UTF-8 + overlong NUL" encoding, a bit like
Java's modified UTF-8 is "CESU-8 + overlong NUL".
-}
newtype ShortText = ShortText { contents :: SBS.ShortByteString
                              }
                              deriving newtype (Eq, Binary, Semigroup, Monoid, NFData)

instance Show ShortText where
  show = show . unpack

instance Ord ShortText where
  -- Must compare Modified UTF-8 codepoins!
  -- Stock deriving Ord is subtly incorrect!
  compare = utf8CompareShortByteString `on` contents

-- We don't want to derive this one from ShortByteString since that one won't handle
-- UTF-8 characters correctly.
instance IsString ShortText where
  fromString = pack

-- | /O(n)/ Returns the length of the 'ShortText' in characters.
codepointLength :: ShortText -> Int
codepointLength st = utf8CountCharsShortByteString (contents st)

-- | /O(1)/ Returns the length of the 'ShortText' in bytes.
byteLength :: ShortText -> Int
byteLength st = SBS.length $ contents st

-- | /O(n)/ Convert a 'String' into a 'ShortText'.
pack :: String -> ShortText
pack s = ShortText $ utf8EncodeShortByteString s

-- | Create a singleton
singleton :: Char -> ShortText
singleton s = pack [s]

-- | /O(n)/ Convert a 'ShortText' into a 'String'.
unpack :: ShortText -> String
unpack st = utf8DecodeShortByteString $ contents st

-- | /O(1)/ Test whether the 'ShortText' is the empty string.
null :: ShortText -> Bool
null st = SBS.null $ contents st

-- | /O(n)/ Split a 'ShortText' representing a file path into its components by separating
-- on the file separator characters for this platform.
splitFilePath :: ShortText -> [ShortText]
-- This seems dangerous, but since the path separators are in the ASCII set they map down
-- to a single byte when encoded in UTF-8 and so this should work even when casting to ByteString.
-- We DeepSeq.force the resulting list so that we can be sure that no references to the
-- bytestring in `st'` remain in unevaluated thunks, which might prevent `st'` from being
-- collected by the GC.
splitFilePath st = DeepSeq.force $ map (ShortText . SBS.toShort) $ B8.splitWith isPathSeparator st'
  where st' = SBS.fromShort $ contents st

-- | /O(1)/ Returns the first UTF-8 codepoint in the 'ShortText'. Depending on the string in
-- question, this may or may not be the actual first character in the string due to Unicode
-- non-printable characters.
head :: ShortText -> Char
head st
  | hd:_ <- unpack st
  = hd
  | otherwise
  = error "head: Empty ShortText"

-- | /O(n)/ The 'stripPrefix' function takes two 'ShortText's and returns 'Just' the remainder of
-- the second iff the first is its prefix, and otherwise Nothing.
stripPrefix :: ShortText -> ShortText -> Maybe ShortText
stripPrefix prefix st = do
  let !(SBS.SBS prefixBA) = contents prefix
  let !(SBS.SBS stBA)     = contents st
  let prefixLength        = sizeofByteArray# prefixBA
  let stLength            = sizeofByteArray# stBA
  -- If the length of 'st' is not >= than the length of 'prefix', it is impossible for 'prefix'
  -- to be the prefix of `st`.
  guard $ (I# stLength) >= (I# prefixLength)
  -- 'prefix' is a prefix of 'st' if the first <length of prefix> bytes of 'st'
  -- are equal to 'prefix'
  guard $ I# (compareByteArrays# prefixBA 0# stBA 0# prefixLength) == 0
  -- Allocate a new ByteArray# and copy the remainder of the 'st' into it
  unsafeDupablePerformIO $ do
    let newBAsize = (stLength -# prefixLength)
    newSBS <- IO $ \s0 ->
      let !(# s1, ba #)  = newByteArray# newBAsize s0
          s2             = copyByteArray# stBA prefixLength ba 0# newBAsize s1
          !(# s3, fba #) = unsafeFreezeByteArray# ba s2
      in  (# s3, SBS.SBS fba #)
    return . Just . ShortText $ newSBS

instance Data ShortText where
  toConstr _   = shortTextConstr
  gunfold _ _  = error "ShortText:gunfold"
  dataTypeOf _ = mkNoRepType "ShortText"

shortTextConstr :: Constr
shortTextConstr = mkConstr shortTextDataType ("{abstract:ShortText}") [] Prefix

shortTextDataType :: DataType
shortTextDataType = mkDataType "ShortText" [shortTextConstr]
