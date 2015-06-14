module Settings (
    settings
    ) where

import Targets
import Base hiding (arg, args)
import Settings.GhcPkg
import Settings.GhcCabal
import UserSettings
import Expression hiding (when, liftIO)

settings :: Settings
settings = defaultSettings <> userSettings

defaultSettings :: Settings
defaultSettings = mconcat
    [ cabalSettings
    , ghcPkgSettings
    , customPackageSettings ]
