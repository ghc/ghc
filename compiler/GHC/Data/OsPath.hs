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

import System.OsPath
import System.OsString (isSuffixOf)
import System.Directory.OsPath (doesDirectoryExist, doesFileExist, getDirectoryContents, createDirectoryIfMissing)
import System.Directory.Internal (os)
import System.OsString.Internal.Types (coercionToPlatformTypes, unWS, unPS)
import Data.Type.Coercion (coerceWith)
import Data.ByteString.Short (ShortByteString)

-- | Decode an 'OsPath' to 'FilePath', throwing an 'error' if decoding failed.
-- Prefer 'decodeUtf' and gracious error handling.
unsafeDecodeUtf :: HasCallStack => OsPath -> FilePath
unsafeDecodeUtf p =
  either (\err -> panic $ "Failed to decodeUtf \"" ++ show p ++ "\", because: " ++ show err) id (decodeUtf p)

unOS :: OsString -> ShortByteString
unOS os = case coercionToPlatformTypes of
    Left (_, windowsStringEv) ->  unWS $ coerceWith windowsStringEv os
    Right (_, posixStringEv) -> unPS $ coerceWith posixStringEv os
