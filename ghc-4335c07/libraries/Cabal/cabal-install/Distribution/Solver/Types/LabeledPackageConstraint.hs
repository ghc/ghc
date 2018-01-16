module Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint(..)
    , unlabelPackageConstraint
    ) where

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageConstraint

-- | 'PackageConstraint' labeled with its source.
data LabeledPackageConstraint
   = LabeledPackageConstraint PackageConstraint ConstraintSource

unlabelPackageConstraint :: LabeledPackageConstraint -> PackageConstraint
unlabelPackageConstraint (LabeledPackageConstraint pc _) = pc
