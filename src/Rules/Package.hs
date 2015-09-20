module Rules.Package (buildPackage) where

import Base
import Rules.Compile
import Rules.Data
import Rules.Dependencies
import Rules.Documentation
import Rules.Generate
import Rules.Library
import Rules.Resources
import Target

buildPackage :: Resources -> PartialTarget -> Rules ()
buildPackage = mconcat
    [ buildPackageData
    , buildPackageDependencies
    , generatePackageCode
    , compilePackage
    , buildPackageLibrary
    , buildPackageDocumentation ]
