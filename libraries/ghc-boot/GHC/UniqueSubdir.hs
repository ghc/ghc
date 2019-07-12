module GHC.UniqueSubdir
  ( uniqueSubdir
  ) where

import Prelude -- See Note [Why do we import Prelude here?]

import Data.List (intercalate)

import GHC.Platform
import GHC.Version (cProjectVersion)

-- | A filepath like @x86_64-linux-7.6.3@ with the platform string to use when
-- constructing platform-version-dependent files that need to co-exist.
--
-- 'ghc-pkg' falls back on the host platform if the settings file is missing,
-- and so needs this since we don't have information about the host platform in
-- as much detail as 'Platform', so we use 'PlatformMini' instead.
uniqueSubdir :: PlatformMini -> FilePath
uniqueSubdir archOs = intercalate "-"
  [ stringEncodeArch $ platformMini_arch archOs
  , stringEncodeOS $ platformMini_os archOs
  , cProjectVersion
  ]
  -- NB: This functionality is reimplemented in Cabal, so if you
  -- change it, be sure to update Cabal.
  -- TODO make Cabal use this now that it is in ghc-boot.
