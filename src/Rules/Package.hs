module Rules.Package (buildPackage) where

import Base
import Expression
import Rules.Data
import Rules.Compile
import Rules.Library
import Rules.Resources
import Rules.Dependencies

buildPackage :: Resources -> StagePackageTarget -> Rules ()
buildPackage = mconcat
    [ buildPackageData
    , buildPackageDependencies
    , compilePackage
    , buildPackageLibrary ]
