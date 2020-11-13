{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, GeneralizedNewtypeDeriving, DerivingStrategies, CPP #-}
{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
-- gross hack: we manuvered ourselves into a position where we can't boot GHC with a LLVM based GHC anymore.
-- LLVM based GHC's fail to compile memcmp ffi calls.  These end up as memcmp$def in the llvm ir, however we
-- don't have any prototypes and subsequently the llvm toolchain chokes on them.  Since 7fdcce6d, we use
-- ShortText for the package database.  This however introduces this very module; which through inlining ends
-- up bringing memcmp_ByteArray from bytestring:Data.ByteString.Short.Internal into scope, which results in
-- the memcmp call we choke on.
--
-- The solution thusly is to force late binding via the linker instead of inlining when comping with the
-- bootstrap compiler.  This will produce a slower (slightly less optimised) stage1 compiler only.
--
-- See issue 18857. hsyl20 deserves credit for coming up with the idea for the soltuion.
--
-- This can be removed when we exit the boot compiler window. Thus once we drop GHC-9.2 as boot compiler,
-- we can drop this code as well.
#if GHC_STAGE < 1
{-# OPTIONS_GHC -fignore-interface-pragmas #-}
#endif
-- |
-- An Unicode string for internal GHC use. Meant to replace String
-- in places where being a lazy linked is not very useful and a more
-- memory efficient data structure is desirable.

-- Very similar to FastString, but not hash-consed and with some extra instances and
-- functions for serialisation and I/O. Should be imported qualified.

module GHC.Data.ShortText (
        -- * ShortText
        ShortText(..),
        -- ** Conversion to and from String
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
import Data.Binary
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short.Internal as SBS
import GHC.Exts
import GHC.IO
import GHC.Utils.Encoding
import System.FilePath (isPathSeparator)

{-| A 'ShortText' is a modified UTF-8 encoded string meant for short strings like
file paths, module descriptions, etc.
-}
newtype ShortText = ShortText { contents :: SBS.ShortByteString
                              }
                              deriving stock (Show)
                              deriving newtype (Eq, Ord, Binary, Semigroup, Monoid, NFData)

-- We don't want to derive this one from ShortByteString since that one won't handle
-- UTF-8 characters correctly.
instance IsString ShortText where
  fromString = pack

-- | /O(n)/ Returns the length of the 'ShortText' in characters.
codepointLength :: ShortText -> Int
codepointLength st = unsafeDupablePerformIO $ countUTF8Chars (contents st)
-- | /O(1)/ Returns the length of the 'ShortText' in bytes.
byteLength :: ShortText -> Int
byteLength st = SBS.length $ contents st

-- | /O(n)/ Convert a 'String' into a 'ShortText'.
pack :: String -> ShortText
pack s = unsafeDupablePerformIO $ ShortText <$> utf8EncodeShortByteString s

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
  | SBS.null $ contents st = error "head: Empty ShortText"
  | otherwise              = Prelude.head $ unpack st

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
