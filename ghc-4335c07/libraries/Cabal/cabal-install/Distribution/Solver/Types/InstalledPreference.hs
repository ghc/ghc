module Distribution.Solver.Types.InstalledPreference
    ( InstalledPreference(..),
    ) where

-- | Whether we prefer an installed version of a package or simply the latest
-- version.
--
data InstalledPreference = PreferInstalled | PreferLatest
  deriving Show
