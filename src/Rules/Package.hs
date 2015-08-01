module Rules.Package (
    buildPackage
    ) where

import Base
import Expression
import Rules.Data
import Rules.Dependencies

buildPackage :: StagePackageTarget -> Rules ()
buildPackage = buildPackageData <> buildPackageDependencies
