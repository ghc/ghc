module Distribution.Solver.Modular.Configured
    ( CP(..)
    ) where

import Distribution.PackageDescription (FlagAssignment)

import Distribution.Solver.Modular.Package
import Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import Distribution.Solver.Types.OptionalStanza

-- | A configured package is a package instance together with
-- a flag assignment and complete dependencies.
data CP qpn = CP (PI qpn) FlagAssignment [OptionalStanza] (ComponentDeps [PI qpn])
