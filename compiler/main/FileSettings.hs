module FileSettings
  ( FileSettings (..)
  ) where

import GhcPrelude

-- | Paths to various files and directories used by GHC, including those that
-- provide more settings.
data FileSettings = FileSettings
  { fileSettings_ghcUsagePath        :: FilePath       -- ditto
  , fileSettings_ghciUsagePath       :: FilePath       -- ditto
  , fileSettings_toolDir             :: Maybe FilePath -- ditto
  , fileSettings_topDir              :: FilePath       -- ditto
  , fileSettings_tmpDir              :: String      -- no trailing '/'
  , fileSettings_systemPackageConfig :: FilePath
  }
