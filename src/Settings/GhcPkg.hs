module Settings.GhcPkg (
    ghcPkgArgs
    ) where

import Util
import Builder
import Switches
import Expression
import Settings.Util
import Settings.GhcCabal
import Settings.TargetDirectory

ghcPkgArgs :: Args
ghcPkgArgs = do
    stage <- getStage
    pkg   <- getPackage
    builder (GhcPkg stage) ? mconcat
        [ arg "update"
        , arg "--force"
        , stage0 ? bootPackageDbArgs
        , arg $ targetPath stage pkg -/- "inplace-pkg-config" ]
