module Rules.Package (
    buildPackage
    ) where

import Base
import Rules.Data
import Expression
import Expression.Settings

buildPackage :: Environment -> Ways -> Settings -> Rules ()
buildPackage = buildPackageData
