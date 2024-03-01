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
  -- * Common utility functions
  , (</>)
  , (<.>)
  , splitSearchPath
  , splitExtension
  , isRelative
  , makeRelative
  , normalise
  , dropTrailingPathSeparator
  , takeDirectory
  , OS.isSuffixOf
  , OS.drop
  , doesDirectoryExist
  , doesFileExist
  , getDirectoryContents
  , createDirectoryIfMissing
  , pprOsPath
  )
  where

import GHC.Prelude

import GHC.Utils.Misc (HasCallStack)
import GHC.Utils.Outputable qualified as Outputable
import GHC.Utils.Panic (panic)

import System.OsPath
import qualified System.OsString as OS (isSuffixOf, drop)
import System.Directory.OsPath (doesDirectoryExist, doesFileExist, getDirectoryContents, createDirectoryIfMissing)
import GHC.Utils.Panic (panic)

import System.OsPath
import System.Directory.Internal (os)

-- | Decode an 'OsPath' to 'FilePath', throwing an 'error' if decoding failed.
-- Prefer 'decodeUtf' and gracious error handling.
unsafeDecodeUtf :: HasCallStack => OsPath -> FilePath
unsafeDecodeUtf p =
  either (\err -> panic $ "Failed to decodeUtf \"" ++ show p ++ "\", because: " ++ show err) id (decodeUtf p)

pprOsPath :: HasCallStack => OsPath -> Outputable.SDoc
pprOsPath = Outputable.text . unsafeDecodeUtf
