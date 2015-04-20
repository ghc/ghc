module Rules.Package (
    buildPackage
    ) where

import Base
import Package
import Rules.Data
import Expression.Base

buildPackage :: Stage -> Package -> FilePath -> Ways -> Settings -> Rules ()
buildPackage = buildPackageData
