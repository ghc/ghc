module Settings (
    settings
    ) where

import Base hiding (arg, args)
import Oracles.Builder
import Settings.GhcPkg
import Settings.GhcCabal
import UserSettings
import Expression hiding (when, liftIO)

settings :: Settings
settings = defaultSettings <> userSettings

defaultSettings :: Settings
defaultSettings = do
    stage <- asks getStage
    mconcat [ builder GhcCabal ? cabalSettings
            , builder (GhcPkg stage) ? ghcPkgSettings ]
