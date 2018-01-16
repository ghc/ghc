module Distribution.Solver.Types.PackageFixedDeps
    ( PackageFixedDeps(..)
    ) where

import           Distribution.InstalledPackageInfo ( InstalledPackageInfo )
import           Distribution.Package
                   ( Package(..), UnitId, installedDepends)
import           Distribution.Solver.Types.ComponentDeps ( ComponentDeps )
import qualified Distribution.Solver.Types.ComponentDeps as CD

-- | Subclass of packages that have specific versioned dependencies.
--
-- So for example a not-yet-configured package has dependencies on version
-- ranges, not specific versions. A configured or an already installed package
-- depends on exact versions. Some operations or data structures (like
--  dependency graphs) only make sense on this subclass of package types.
--
class Package pkg => PackageFixedDeps pkg where
  depends :: pkg -> ComponentDeps [UnitId]

instance PackageFixedDeps InstalledPackageInfo where
  depends pkg = CD.fromInstalled (installedDepends pkg)

