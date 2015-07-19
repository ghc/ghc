module Settings.GhcPkg (
    ghcPkgArgs
    ) where

import Builder
import Switches
import Expression
import Settings.Util
import Settings.GhcCabal
import Settings.TargetDirectory
import Development.Shake.FilePath

ghcPkgArgs :: Args
ghcPkgArgs = do
    pkg <- asks getPackage
    stage <- asks getStage
    builder (GhcPkg stage) ? mconcat
        [ arg "update"
        , arg "--force"
        , stage0 ? bootPackageDbArgs
        , argPath $ targetPath stage pkg </> "inplace-pkg-config" ]
