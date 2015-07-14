module Settings.GhcPkg (
    ghcPkgArgs
    ) where

import Base
import Switches
import Expression hiding (when, liftIO)
import Settings.Util
import Oracles.Builder
import Settings.GhcCabal
import Settings.TargetDirectory

ghcPkgArgs :: Args
ghcPkgArgs = do
    pkg <- asks getPackage
    stage <- asks getStage
    builder (GhcPkg stage) ? mconcat
        [ arg "update"
        , arg "--force"
        , stage0 ? bootPackageDbArgs
        , arg $ targetPath stage pkg </> "inplace-pkg-config" ]
