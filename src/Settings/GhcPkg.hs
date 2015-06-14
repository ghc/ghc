module Settings.GhcPkg (
    ghcPkgSettings
    ) where

import Base hiding (arg, args)
import Package
import Targets
import Expression hiding (when, liftIO)
import Settings.Util
import Settings.GhcCabal

ghcPkgSettings :: Settings
ghcPkgSettings = do
    stg <- asks getStage
    pkg <- asks getPackage
    let dir = pkgPath pkg </> targetDirectory stg pkg
    mconcat [ arg "update"
            , arg "--force"
            , stage Stage0 ? bootPackageDbSettings
            , arg $ dir </> "inplace-pkg-config" ]
