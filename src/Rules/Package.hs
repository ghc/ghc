module Rules.Package (buildPackage) where

import Base
import Expression
import Rules.Compile
import Rules.Data
import Rules.Dependencies
import Rules.Documentation
import Rules.Library
import Rules.Resources
import Target

buildPackage :: Resources -> PartialTarget -> Rules ()
buildPackage = mconcat
    [ buildPackageData
    , buildPackageDependencies
    , compilePackage
    , buildPackageLibrary
    , buildPackageDocumentation ]
