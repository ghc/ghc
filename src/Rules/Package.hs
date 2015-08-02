module Rules.Package (buildPackage) where

import Base
import Expression
import Rules.Data
import Rules.Resources
import Rules.Dependencies

buildPackage :: Resources -> StagePackageTarget -> Rules ()
buildPackage = buildPackageData <> buildPackageDependencies
