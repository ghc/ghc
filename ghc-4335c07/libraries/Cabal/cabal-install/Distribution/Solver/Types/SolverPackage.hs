{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.SolverPackage
    ( SolverPackage(..)
    ) where

import Distribution.Compat.Binary (Binary(..))
import Distribution.Package ( Package(..) )
import Distribution.PackageDescription ( FlagAssignment )
import Distribution.Solver.Types.ComponentDeps ( ComponentDeps )
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.SolverId
import Distribution.Solver.Types.SourcePackage
import GHC.Generics (Generic)

-- | A 'SolverPackage' is a package specified by the dependency solver.
-- It will get elaborated into a 'ConfiguredPackage' or even an
-- 'ElaboratedConfiguredPackage'.
--
-- NB: 'SolverPackage's are essentially always with 'UnresolvedPkgLoc',
-- but for symmetry we have the parameter.  (Maybe it can be removed.)
--
data SolverPackage loc = SolverPackage {
        solverPkgSource :: SourcePackage loc,
        solverPkgFlags :: FlagAssignment,
        solverPkgStanzas :: [OptionalStanza],
        solverPkgLibDeps :: ComponentDeps [SolverId],
        solverPkgExeDeps :: ComponentDeps [SolverId]
    }
  deriving (Eq, Show, Generic)

instance Binary loc => Binary (SolverPackage loc)

instance Package (SolverPackage loc) where
  packageId = packageId . solverPkgSource
