module Rules.Package (buildPackage) where

import Base
import Target
import Expression
import Rules.Data
import Rules.Compile
import Rules.Library
import Rules.Resources
import Rules.Dependencies
import Rules.Documentation

buildPackage :: Resources -> PartialTarget -> Rules ()
buildPackage = mconcat
    [ buildPackageData
    , buildPackageDependencies
    , compilePackage
    , buildPackageLibrary
    , buildPackageDocumentation ]
