module Rules.Package (
    buildPackage
    ) where

import Rules.Data
import Expression
import Development.Shake

buildPackage :: StagePackageTarget -> Rules ()
buildPackage = buildPackageData
