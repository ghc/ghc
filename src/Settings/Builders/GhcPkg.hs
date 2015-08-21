module Settings.Builders.GhcPkg (ghcPkgArgs) where

import Util
import Builder
import Expression
import Predicates
import Settings.Util
import Settings.Builders.GhcCabal

ghcPkgArgs :: Args
ghcPkgArgs = stagedBuilder GhcPkg ? do
    path <- getTargetPath
    mconcat [ arg "update"
            , arg "--force"
            , stage0 ? bootPackageDbArgs
            , arg $ path -/- "inplace-pkg-config" ]
