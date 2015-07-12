module Rules.Package (
    buildPackage
    ) where

import Base
import Rules.Data
import Expression

buildPackage :: Target -> Rules ()
buildPackage = buildPackageData
