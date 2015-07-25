module Rules.Package (
    buildPackage
    ) where

import Expression
import Rules.Data
import Rules.Dependencies
import Development.Shake

buildPackage :: StagePackageTarget -> Rules ()
buildPackage = buildPackageData <> buildPackageDependencies
