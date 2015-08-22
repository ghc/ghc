module Settings.Builders.GhcPkg (ghcPkgArgs) where

import Expression
import Predicates
import Settings
import Settings.Builders.GhcCabal

ghcPkgArgs :: Args
ghcPkgArgs = stagedBuilder GhcPkg ? do
    path <- getTargetPath
    mconcat [ arg "update"
            , arg "--force"
            , stage0 ? bootPackageDbArgs
            , arg $ path -/- "inplace-pkg-config" ]
