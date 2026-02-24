module GHC.Data.OsPath
  (
  -- * OsPath initialisation and transformation
    OsPath
  , OsString
  , encodeUtf
  , decodeUtf
  , unsafeDecodeUtf
  , unsafeEncodeUtf
  , os
  , unOS
  -- * Common utility functions
  , (</>)
  , (<.>)
  , splitSearchPath
  , isRelative
  , dropTrailingPathSeparator
  , takeDirectory
  , isSuffixOf
  , doesDirectoryExist
  , doesFileExist
  , getDirectoryContents
  , createDirectoryIfMissing
  )
  where

import GHC.Prelude

import GHC.Utils.Misc (HasCallStack)
import GHC.Utils.Panic (panic)

import Data.ByteString.Short (ShortByteString)
import Data.Type.Coercion (coerceWith)
import System.Directory.Internal (os)
import System.Directory.OsPath (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.OsPath
import System.OsString (isSuffixOf)
import System.OsString.Internal.Types (coercionToPlatformTypes, unPS, unWS)

-- | Decode an 'OsPath' to 'FilePath', throwing an 'error' if decoding failed.
-- Prefer 'decodeUtf' and gracious error handling.
unsafeDecodeUtf :: HasCallStack => OsPath -> FilePath
unsafeDecodeUtf p =
  either (\err -> panic $ "Failed to decodeUtf \"" ++ show p ++ "\", because: " ++ show err) id (decodeUtf p)

-- | Extracts underlying 'ShortByteString' from the given 'OsString', taking care of platform specifics.
unOS :: OsString -> ShortByteString
unOS os = case coercionToPlatformTypes of
  Left (_, windowsStringEv) -> unWS $ coerceWith windowsStringEv os
  Right (_, posixStringEv) -> unPS $ coerceWith posixStringEv os
