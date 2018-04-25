{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.ResolverPackage
    ( ResolverPackage(..)
    , resolverPackageLibDeps
    , resolverPackageExeDeps
    ) where

import Distribution.Solver.Types.InstSolverPackage
import Distribution.Solver.Types.SolverId
import Distribution.Solver.Types.SolverPackage
import qualified Distribution.Solver.Types.ComponentDeps as CD

import Distribution.Compat.Binary (Binary(..))
import Distribution.Compat.Graph (IsNode(..))
import Distribution.Package (Package(..), HasUnitId(..))
import Distribution.Simple.Utils (ordNub)
import GHC.Generics (Generic)

-- | The dependency resolver picks either pre-existing installed packages
-- or it picks source packages along with package configuration.
--
-- This is like the 'InstallPlan.PlanPackage' but with fewer cases.
--
data ResolverPackage loc = PreExisting InstSolverPackage
                         | Configured  (SolverPackage loc)
  deriving (Eq, Show, Generic)

instance Binary loc => Binary (ResolverPackage loc)

instance Package (ResolverPackage loc) where
  packageId (PreExisting ipkg)     = packageId ipkg
  packageId (Configured  spkg)     = packageId spkg

resolverPackageLibDeps :: ResolverPackage loc -> CD.ComponentDeps [SolverId]
resolverPackageLibDeps (PreExisting ipkg) = instSolverPkgLibDeps ipkg
resolverPackageLibDeps (Configured spkg) = solverPkgLibDeps spkg

resolverPackageExeDeps :: ResolverPackage loc -> CD.ComponentDeps [SolverId]
resolverPackageExeDeps (PreExisting ipkg) = instSolverPkgExeDeps ipkg
resolverPackageExeDeps (Configured spkg) = solverPkgExeDeps spkg

instance IsNode (ResolverPackage loc) where
  type Key (ResolverPackage loc) = SolverId
  nodeKey (PreExisting ipkg) = PreExistingId (packageId ipkg) (installedUnitId ipkg)
  nodeKey (Configured spkg) = PlannedId (packageId spkg)
  -- Use dependencies for ALL components
  nodeNeighbors pkg =
    ordNub $ CD.flatDeps (resolverPackageLibDeps pkg) ++
             CD.flatDeps (resolverPackageExeDeps pkg)
