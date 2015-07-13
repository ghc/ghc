module Settings.GhcPkg (
    ghcPkgSettings
    ) where

import Base hiding (arg, args)
import Switches
import Expression hiding (when, liftIO)
import Settings.Util
import Oracles.Builder
import Settings.GhcCabal
import Settings.TargetDirectory

ghcPkgSettings :: Settings
ghcPkgSettings = do
    pkg <- asks getPackage
    stage <- asks getStage
    builder (GhcPkg stage) ? mconcat
        [ arg "update"
        , arg "--force"
        , stage0 ? bootPackageDbSettings
        , arg $ targetPath stage pkg </> "inplace-pkg-config" ]
