module Rules.Package (buildPackage) where

import Base
import Context
import qualified Rules.Compile
import qualified Rules.Data
import qualified Rules.Dependencies
import qualified Rules.Documentation
import qualified Rules.Generate
import qualified Rules.Library
import qualified Rules.Program
import qualified Rules.Register
import Rules.Resources

buildPackage :: Resources -> Context -> Rules ()
buildPackage = mconcat
    [ Rules.Compile.compilePackage
    , Rules.Data.buildPackageData
    , Rules.Dependencies.buildPackageDependencies
    , Rules.Documentation.buildPackageDocumentation
    , Rules.Generate.generatePackageCode
    , Rules.Library.buildPackageLibrary
    , Rules.Program.buildProgram
    , Rules.Register.registerPackage ]
