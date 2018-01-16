module Distribution.Solver.Types.PackagePreferences
    ( PackagePreferences(..)
    ) where

import Distribution.Solver.Types.InstalledPreference
import Distribution.Solver.Types.OptionalStanza
import Distribution.Version (VersionRange)

-- | Per-package preferences on the version. It is a soft constraint that the
-- 'DependencyResolver' should try to respect where possible. It consists of
-- an 'InstalledPreference' which says if we prefer versions of packages
-- that are already installed. It also has (possibly multiple)
-- 'PackageVersionPreference's which are suggested constraints on the version
-- number. The resolver should try to use package versions that satisfy
-- the maximum number of the suggested version constraints.
--
-- It is not specified if preferences on some packages are more important than
-- others.
--
data PackagePreferences = PackagePreferences [VersionRange]
                                             InstalledPreference
                                             [OptionalStanza]
