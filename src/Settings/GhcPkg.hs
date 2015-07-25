module Settings.GhcPkg (ghcPkgArgs) where

import Util
import Builder
import Switches
import Expression
import Settings.Util
import Settings.GhcCabal

ghcPkgArgs :: Args
ghcPkgArgs = stagedBuilder GhcPkg ? do
    path <- getTargetPath
    mconcat [ arg "update"
            , arg "--force"
            , stage0 ? bootPackageDbArgs
            , arg $ path -/- "inplace-pkg-config" ]
