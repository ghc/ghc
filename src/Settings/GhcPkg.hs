module Settings.GhcPkg (
    ghcPkgSettings
    ) where

import Base hiding (arg, args)
import Package
import Targets
import Switches
import Expression hiding (when, liftIO)
import Settings.Util
import Oracles.Builder
import Settings.GhcCabal

ghcPkgSettings :: Settings
ghcPkgSettings = do
    pkg <- asks getPackage
    stage <- asks getStage
    let dir = pkgPath pkg </> targetDirectory stage pkg
    builder (GhcPkg stage) ? mconcat
        [ arg "update"
        , arg "--force"
        , stage0 ? bootPackageDbSettings
        , arg $ dir </> "inplace-pkg-config" ]
